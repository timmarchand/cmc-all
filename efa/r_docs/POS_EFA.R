
## set libraries
library(tidyverse)
library(gsubfn)
library(pysch)
## set date
today <- Sys.Date()

##import the file from POS project
master <- read_csv("./data/2020-02-26_full_freq_table_no_cut.csv")
## remove duplicates
master <- master %>% distinct()

# add corpus column
master$corpus <- gsubfn("([A-Z]{3}).*","\\1",master$file_id,perl=TRUE)

#  NEWS:  98 files > 1000 wc
master %>% select(corpus,file_id,pos,wordcount) %>%
  filter(corpus == "NWS") %>% arrange(wordcount) %>% 
  filter(wordcount > 1000) -> news_select

##  ACA: 114 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "ACA") %>% arrange(wordcount) %>%  
  filter(wordcount > 1000) -> aca_select

##  FIC: 10000 > 189 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "FIC") %>% arrange(wordcount) %>%  
  filter(wordcount < 10000 , wordcount > 1000) -> fic_select

##  DEM: 265 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "DEM") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> dem_select

## HYC: 332 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "HYC") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> hyc_select

##  JUC: 103 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "JUC") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> juc_select

pos_for_efa <- rbind(aca_select,dem_select,fic_select,news_select,hyc_select,juc_select)

rm(list=c("aca_select","dem_select","fic_select","news_select","hyc_select","juc_select"))

pos_for_efa %>% group_by(corpus) %>% 
  summarise (count = n(), mean = mean(wordcount),sd = sd(wordcount),
             min_wc =min(wordcount),max_wc = max(wordcount),
             total = sum(wordcount)) -> pos_for_efa_table



##accuracy
summary(master)

# make freq table
master1 <- master %>% tidyr::pivot_wider(names_from = ngrams, # ngrams become the column names
                   values_from = prop)

##missing
percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)

##exclude the participant missing too much data
replacepeople <- subset(master, missing <= 5)

##make sure the columns aren't missing too much
apply(replacepeople, 2, percentmissing)

# ##replace away!
# library(mice)
# tempnomiss <- mice(replacepeople)
# nomiss <- complete(tempnomiss, 1)
# summary(nomiss)

##outliers
cutoff <- qchisq(1-.001, ncol(nomiss))
mahal <- mahalanobis(nomiss,
                    colMeans(nomiss),
                    cov(nomiss))
cutoff ##cutoff score
ncol(nomiss) ##df
summary(mahal < cutoff)

##exclude outliers
noout <- subset(nomiss, mahal < cutoff)

##additivity
correl <- cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random <- rchisq(nrow(noout), 4)
fake <- lm(random~., data = noout)
standardized <- rstudent(fake)
fitted <- scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the efa analysis
library(psych)
library(GPArotation)
library(parameters)

# test for KMO and Bartlett
check_factorstructure(master)

# find optimal number of factors
nf <- parameters::n_factors(master)
nf


efa <- psych::fa(master, nfactors = 5) %>% 
  model_parameters(sort = TRUE, threshold = "max")
efa



##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors <- fa.parallel(noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 <- fa(noout, nfactors=3, rotate = "oblimin", fm = "ml")
round1

round2 <- fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
round2

##get cfi
finalmodel <- fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 <- c(1, 3, 7, 8, 10:12, 14, 16, 18, 20:25, 29, 31, 32)
factor2 <- c(2, 5, 13, 19, 26, 28, 30)
factor3 <- c(6, 9, 17, 27)
psych::alpha(noout[ , factor1])
psych::alpha(noout[ , factor2])
psych::alpha(noout[ , factor3])

##create new factor scores
noout$f1 <- apply(noout[ , factor1], 1, mean) ##creates average scores
noout$f2 <- apply(noout[ , factor2], 1, mean) ##creates average scores
noout$f3 <- apply(noout[ , factor3], 1, mean) ##creates average scores

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)
