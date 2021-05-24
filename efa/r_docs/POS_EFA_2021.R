
## set libraries ------
library(tidyverse)
library(gsubfn)
library(pysch)
library(GGally)
library(psych)
library(GPArotation)
library(parameters)
library(corrplot)
library(caret)
## set date
today <- Sys.Date()

# Functions -----

make_matrix <- function(x){
  x %>% select(-c(2))  %>% column_to_rownames(var = "file") %>% select(where(~ any(. != 0)))
}

get_loadings <- function(x, cut = NULL) {
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



# Get data ----------------------------------------------------------------

##import the file from POS project
master <- read_csv(here::here("efa","data","efa_table.csv"))

##import the file from POS project and change input to matrices
master_table <- readRDS(here::here("efa","data","efa_input_table.rds")) %>% 
  mutate(data_efa_10 = map(.x= data_efa_10, .f = ~make_matrix(.x))) %>% 
  mutate(data_efa_25 = map(.x= data_efa_25, .f = ~make_matrix(.x))) %>% 
  mutate(data_efa_50 = map(.x= data_efa_50, .f = ~make_matrix(.x))) 

## import data from efa folder rds
master_nest <- readRDS("efa/data/efa_all.rds") %>% 
  mutate(threshold = parse_number(type) , .after = type)

## change dfs to matrices, get correlation matrix, determinants and no of factors recommended details from each

efa_12_600 <- master_nest %>% 
  filter(top == 12, threshold =600) %>% 
  mutate(matrix = map(data, ~make_matrix(.x)))  %>%
  mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
  mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
  mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
  mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble))

get_cor_details <- function(pos = "gram", count = 50, cut = 600{
  master_nest %>% 
    filter(str_detect(type, pos)) %>% 
    filter(top == count, threshold == cut) %>% 
    mutate(matrix = map(data, ~make_matrix(.x)))  %>%
    mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>%
    mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>%
    mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>%
    mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble)) %>% 
    mutate(n_factors = set_names(n_factors, paste0(type,"_",top)))
}
     get_cor_details(pos = "tri", count = 12, cut = c(600,1000)) 
     
     master_nest %>% 
       filter(top == 50, threshold == 600) %>% 
       filter(str_detect(type, "uni")) %>% 
       mutate(matrix = map(data, ~make_matrix(.x)))  %>%
       mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
       mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
       mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
       mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble))

master_details <- master_nest %>% filter(top <100) %>% #slice(13) %>% 
  mutate(matrix = map(data, ~make_matrix(.x)))  %>%
  mutate(cor_mat = map(.x = matrix, .f = ~.x %>% cor)) %>% 
  mutate(determinant = map_dbl(.x = matrix, .f = ~.x %>% cor %>% det)) %>% 
  mutate(symnum = map(.x = cor_mat, .f = ~ .x %>% symnum)) %>% 
  mutate(n_factors = map(.x = matrix, .f = ~parameters::n_factors(.x) %>% tibble)) %>% 
  mutate(n_factors = set_names(n_factors, paste0(type,"_",top)))


## save the master_details file

master_details %>% saveRDS(here::here("efa","data","master_details.rds"))

master_details %>% arrange(-determinant) %>% 
  pluck("n_factors",1)

# ##missing %>% 
# percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
# missing <- apply(master, 1, percentmissing)
# table(missing)
# 
# 
# ##outliers
# cutoff <- qchisq(1-.001, ncol(master))
# 
# ## make master a matrix
# master_matrix <- master %>% select(-c(2)) %>% column_to_rownames(var = "file")
# 
# mahal <- mahalanobis(master_matrix,
#                     colMeans(master_matrix),
#                     cov(master_matrix))
# cutoff ##cutoff score
# ncol(master_matrix) ##df
# summary(mahal < cutoff)

## find the determinant of the correlation matrix
## det > 0.00001 advised to avoid multicolinearity effects

master_nest %>% mutate(determinants = )



 determinants <- master_table %>% 
  transmute(threshold,
            det_10 = map_dbl(.x = data_efa_10, .f = ~.x %>% cor %>% det),
            det_25 = map_dbl(.x = data_efa_25, .f = ~.x %>% cor %>% det),
            det_50 = map_dbl(.x = data_efa_50, .f = ~.x %>% cor %>% det))
 
 
 corr_matrices <- master_table %>% 
   transmute(threshold,
             cor_10 = map(.x = data_efa_10, .f = ~.x %>% cor),
             cor_25 = map(.x = data_efa_25, .f = ~.x %>% cor),
             cor_50 = map(.x = data_efa_50, .f = ~.x %>% cor))
 
 corr_matrices %>% pluck("cor_10", 4) %>% car::vif()
 
 ## visualise correlations
 cor_symnums <- master_table %>% 
   transmute(threshold,
             sym_10 = map(.x = data_efa_10, .f = ~.x %>% cor %>% symnum),
             sym_25 = map(.x = data_efa_25, .f = ~.x %>% cor %>% symnum),
             sym_50 = map(.x = data_efa_50, .f = ~.x %>% cor %>% symnum))

 ## example of saving data
 cor_symnums %>% pluck("sym_10", 4) %>% write.csv(here::here("efa","data","sym_num_10_600.csv"))

#  
# ##exclude outliers
# no_out <- subset(master_matrix, mahal < cutoff)
# 
# ##additivity
# correl <- cor(no_out, use = "pairwise.complete.obs")
# symnum(correl)
# correl
# 
# # create correlation tables
# master_corrs <- master_table %>% 
#   transmute(threshold,
#            correl_10 = 
#            map(.x = data_efa_10, .f = ~cor(.x,use="pairwise.complete.obs") %>% symnum(.)),
#            correl_25 = 
#            map(.x = data_efa_25, .f = ~cor(.x,use="pairwise.complete.obs")),
#            correl_50 = 
#            map(.x = data_efa_50, .f = ~cor(.x,use="pairwise.complete.obs"))) %>% 
#   pluck("correl_10",1)
# 

##assumption set up
 
test_assumptions <- function(x){
  test = list()
  random <- rchisq(nrow(x), df = 4) # degrees of freedom = 4?
  # generate fake random data
  fake <- lm(random~., data = x)
  standardized <- rstudent(fake)
  fitted <- scale(fake$fitted.values)
  
 ##normality
hist <- hist(standardized)
 
 ##linearity
q <- qqnorm(standardized) + abline(0,1)
 
 ##homogeneity
 p <- plot(fitted,standardized)
 p <- p + abline(0,0)
 P <- p  +abline(v = 0)

 test[[1]] <- hist
 test[[2]] <- qqnorm
 test[[3]] <- p
 return(test)
}

df %>% test_assumptions() -> asd


## test normality of multivariates
MVN::mvn(df, mvnTest = "mardia")


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





  
  1 - ((finalmodel$STATISTIC-finalmodel$dof)/
         (finalmodel$null.chisq-finalmodel$null.dof))

tbl <- tibble(AIC =  x %>% pluck("AIC"),
              BIC = x %>% pluck("BIC"))

return(tbl)

}

# running fa --------------------------------------------------------------


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

tbl %>% pluck("loadings", 4) %>% select(starts_with("PA"))

tbl %>% filter(factors == 3) %>% pull(fa_corrs)
tbl %>% filter(factors == 9) %>% pull(fa_corrs)

tbl %>% filter(factors == 3) %>% pull(loadings)
tbl %>% filter(factors == 9) %>% pluck(loadings,1) %>% View

tbl %>%  mutate(RMSEA = map_dbl(.x = model, .f = ~pluck(.x,1,"RMSEA")))




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
