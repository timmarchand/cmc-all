
## set libraries ------
library(tidyverse)
library(gsubfn)
library(pysch)
## set date
today <- Sys.Date()

# Functions -----

make_matrix <- function(x){
  x %>% select(-c(2)) %>% column_to_rownames(var = "file")
}


##import the file from POS project
master <- read_csv(here::here("efa","data","efa_table.csv"))

master_table <- readRDS(here::here("efa","data","efa_input_table.rds"))



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
round1$loadings[]
round1$structure[]

fa.parallel(master_matrix, fa = "fa")

round1$BIC

beta <- nest(no_out)
beta<- tibble(factors = 1:5,
              data = nest(no_out))

 beta %>% 
  mutate(model = map2(.x = .$factors, .y = .$data, 
                             .f = ~fa(.y, nfactors = .x, 
                            rotate = "oblimin", fm = "ml"))) %>% 
  mutate(loadings = map(.x = model, .f = ~fa_table(.x, cut = 0.3))) %>% 
  mutate(BIC = map_dbl(.x = model, .f = ~get_BIC(.x)))

beta %>% mutate(tidy = map(model, broom::tidy))

beta %>% mutate(BIC = map(.x = model,  .f = ~pull %>% pluck(1,"BIC"))) %>% unnest(c(BIC))

?pluck%>% 
  mutate(BIC = map_dbl(model, pull(BIC)))




get_loadings <- function(x, cut = 0) {
  #get sorted loadings
  loadings <- fa.sort(x)$loadings %>% round(3)
  #supress loadings
  loadings[loadings < abs(cut)] <- ""
  #get additional info
  add_info <- cbind(x$communalities, 
                    x$uniquenesses,
                    x$complexity) %>%
    # make it a data frame
    as.data.frame() %>%
    # column names
    rename("Communality" = V1,
           "Uniqueness" = V2,
           "Complexity" = V3) %>%
    #get the item names from the vector
    rownames_to_column("POSgram")
  #build table
  loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("POSgram") %>%
    left_join(add_info) %>%
    mutate(across(where(is.numeric), round, 3))
}


get_fa_cors <- function(x){
  x %>% pluck("Phi")
}

beta %>% mutate(fa_cors = map_df(.x = model, .f = ~get_fa_cors(.x)))
get_fa_cors(round1)
round1 %>% pluck("score.cor") %>% as.data.frame() %>% rownames(., prefix ="ML")

get_cfi <- function(x){
  stat <- x %>% pluck("STATISTIC")
  dof <- x %>% pluck("dof")
  null.chisq <- x %>% pluck("null.chisq")
  null.dof <- x %>% pluck("null.dof")
  
  cfi <- (stat-dof)/(null.chisq - null.dof)
  return(cfi)
}
  
  
  1 - ((finalmodel$STATISTIC-finalmodel$dof)/
         (finalmodel$null.chisq-finalmodel$null.dof))

tbl <- tibble(AIC =  x %>% pluck("AIC"),
              BIC = x %>% pluck("BIC"))

return(tbl)

}

## create nested columns
## 
tbl <-  nest(master_matrix,data = everything()) %>% 
  # duplicate data for maximum number of factors 
  slice(rep(1, each=9)) %>% 
  # add factors column
  rowid_to_column(var = "factors") %>% 
  # create list column with fa results for each factor
  mutate(model = map2(.x = .$factors, .y = .$data, 
                      .f = ~fa(.y, nfactors = .x, 
                               rotate = "oblimin", fm = "ml"))) %>% 
  # add BIC information for each model
  mutate(BIC = map_dbl(.x = model, .f = ~get_BIC(.x))) %>% 
  # add loadings table
  mutate(loadings = map(.x = model, .f = ~get_loadings(.x, cut = 0))) %>% 
  # add factor correlations table
  mutate(fa_corrs = map(.x = model, .f = ~get_fa_cors(.x))) %>% 
  # get cfi scores
  mutate(cfi = map_dbl(.x = model, .f = ~get_cfi(.x))) %>% 
  mutate(RMSEA = map_dbl(.x = model, .f = ~pluck(1,"RMSEA")))

tbl %>% filter(factors == 3) %>% pull(fa_corrs)
tbl %>% filter(factors == 9) %>% pull(fa_corrs)

tbl %>% filter(factors == 3) %>% pull(loadings)
tbl %>% filter(factors == 9) %>% pluck(loadings,1) %>% View

tbl %>%  mutate(RMSEA = map_dbl(.x = model, .f = ~pluck(.x,1,"RMSEA")))

get_BIC <- function(x){
  x %>% pluck("BIC")
}

get_RMSEA <- function(x){
  x %>% pluck("RMSEA")
}

get_TLI<- function(x){
  x %>% pluck("TLI")
}

get_cfi <- function(x){
  stat <- x %>% pluck("STATISTIC")
  dof <- x %>% pluck("dof")
  null.chisq <- x %>% pluck("null.chisq")
  null.dof <- x %>% pluck("null.dof")
  
  cfi <- (stat-dof)/(null.chisq - null.dof)
  return(cfi)
}



beta %>% mutate(BIC = map_dbl(model,~get_BIC(.x))) %>% 
         mutate(loadings = map(model, ~fa_loadings(round1, 0.3))) %>% 
          mutate(fa_cors = map_df(.x = model, .f = ~get_fa_cors(.x)))
  


print(round1, cut = .3, digits = 2, sort = TRUE)

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
