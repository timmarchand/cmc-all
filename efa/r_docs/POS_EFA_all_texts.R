##set working directory
setwd("/Users/Tim/Documents/R projects/EFA of POS")
## set libraries
library(tidyverse)
library(gsubfn)
library(pysch)
## set date
today <- Sys.Date()

##import the file from POS project
# master <- read_csv("2020-02-26_full_freq_table_no_cut.csv")
# master <- pivot_table

## use select_df from pos_file_selection script - no texts omiitted

## remove duplicates
master <- master %>% distinct()
rownames(master) <- master$file_id
master %>% ungroup() %>% select(-file_id,-corpus) -> master
##accuracy
summary(master)

### what happens if we take out the DEM data?? ####
master <- pivot_table
master <- master %>% filter(corpus != "DEM") -> master
rownames(master) <- master$file_id
master %>% ungroup() %>% select(-file_id,-corpus) -> master
##missing
percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)

master[is.na(master)] <- 0
apply(master, 2 , function(x) sum(x)) -> sum_cols
master <- master[sum_cols>0]


master[is.na(master)] <- 0
noout <- master

##exclude the ngram missing too much data
replacengram <- subset(master, missing <= 50)

##make sure the columns aren't missing too much
apply(replacengram, 2, percentmissing)

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

##additivity >> note cor() needs to be on a matrix
correl <- cor(master, use = "pairwise.complete.obs")
symnum(correl) -> sym_correl
correl

# send to clipboard?
clip <- pipe("pbcopy", "w")                       
write.table(symnum(correl), file=clip, sep = '\t', row.names = FALSE)                               
close(clip)

##assumption set up
random <- rchisq(nrow(master), 4)
fake <- lm(random~., data = master)
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

##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(master))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors <- fa.parallel(master, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 <- fa(master, nfactors=3, rotate = "oblimin", fm = "ml")
round1

remove <- as.vector(unlist(read.table(pipe("pbpaste"), sep="\t", header=FALSE)))

round2 <- fa(noout[ , -remove], nfactors=3, rotate = "oblimin", fm = "ml")
round2

##get cfi
finalmodel <- fa(noout[ , -remove], nfactors=3, rotate = "oblimin", fm = "ml")
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
