
## set libraries
library(tidyverse)
library(gsubfn)
library(pysch)
## set date
today <- Sys.Date()

##import the file from POS project
master <- read_csv(here::here("efa","data","efa_table.csv"))

##missing
percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)


##outliers
cutoff <- qchisq(1-.001, ncol(master))

## make master a matrix
master_matrix <- master %>% select(-c(2)) %>% column_to_rownames(var = "file")

mahal <- mahalanobis(master_matrix,
                    colMeans(master_matrix),
                    cov(master_matrix))
cutoff ##cutoff score
ncol(master_matrix) ##df
summary(mahal < cutoff)

##exclude outliers
no_out <- subset(master_matrix, mahal < cutoff)

##additivity
correl <- cor(no_out, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random <- rchisq(nrow(no_out), 4)
fake <- lm(random~., data = no_out)
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
check_factorstructure(no_out)

# find optimal number of factors
nf <- parameters::n_factors(no_out)
nf2 <- parameters::n_factors(master_matrix)
nf


efa <- psych::fa(master_matrix, nfactors = 5) %>% 
  model_parameters(sort = TRUE, threshold = "max")
efa



##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(no_out))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors <- fa.parallel(no_out, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 <- fa(no_out, nfactors=3, rotate = "oblimin", fm = "ml")
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
