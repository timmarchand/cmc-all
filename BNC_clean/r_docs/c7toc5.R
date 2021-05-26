library(rvest)
library(tidyverse)
library(rio)
library(magrittr)
library(tidylog)



# functions ---------------------------------------------------------------
## this function takes the wmatrix C7 tags, and removes the markers of pormanteau tags to create a clean_C7 value

## it then further simplifies the C7 tags to allow the C5 tags to be matched when joined

clean_C7 <-  function(df){
  df %>% mutate(alt_C7 = C7,
                alt_C7 = str_remove(alt_C7, "@"),
                alt_C7 = str_remove(alt_C7, "%"),
                clean_C7 = alt_C7,
                alt_C7 = str_remove(alt_C7, "21"),
                alt_C7 = str_remove(alt_C7, "22"),
                alt_C7 = str_remove(alt_C7, "31"),
                alt_C7 = str_remove(alt_C7, "32"),
                alt_C7 = str_remove(alt_C7, "33"),
                alt_C7 = str_remove(alt_C7, "41"),
                alt_C7 = str_remove(alt_C7, "42"),
                alt_C7 = str_remove(alt_C7, "43"),
                alt_C7 = str_remove(alt_C7, "44"),
                alt_C7 = str_remove(alt_C7, "51"),
                alt_C7 = str_remove(alt_C7, "52"),
                alt_C7 = str_remove(alt_C7, "53"),
                alt_C7 = str_remove(alt_C7, "54"),
                alt_C7 = str_remove(alt_C7, "55")) 
}


# data --------------------------------------------------------------------

## get the mapping of C& to C5 tags from wmatrix
url <- "http://ucrel.lancs.ac.uk/claws/mapC7toC5.txt"

## import as dataframe
C7toC5 <- rio::import(url, header = F, quote="" )
## add colnames to match the later join
names(C7toC5) <- c("alt_C7","C5")

## purrr way to load multiple csv files as dataframes¥
## get file path of folder
file_path <- "/Users/timmarchand/Library/Mobile Documents/com~apple~CloudDocs/Wmatrix/csv_output/"

## assign files to variable
file_path %>% list.files() -> csv_file_names

### code for assigning inidividual dfs directly
# csv_file_names %>%
#   purrr::map(function(file_name){ # iterate through each file name
#     assign(x = str_remove(file_name, ".csv"), # Remove file extension ".csv"
#            value = read_csv(paste0(file_path, file_name)),
#            envir = .GlobalEnv) ## assign attributes the result to the Global Environment
#   })
# 

## code to load everything as a list of dataframes

# Load everything into the Global Environment
csv_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    
    read_csv(paste0(file_path, file_name))
    
  }) -> df_list

# Create nested tibble for the corpora

# assign file names as names for each part of the list
df_list <- set_names(df_list , str_remove(csv_file_names,"_result.csv"))

## use enframe to create a nested tibble
corp_dat <- df_list %>% enframe

# change the df colnames
# note the cmc corpus had their tags processed incorrectly in version 1
new_names1 <- c("Text_id","Sentence","Count","C7","Token","Lemma")
new_names2 <- c("Text_id","Sentence","Count","Token","C7","Lemma")


## add colnames to the corp_dat value column
## then apply the clean_C7 function to create two alternatives
## One is for C7 pos analysis
## The other is for joining with C5 mapping df
corp_dat %<>% mutate(value = case_when
                     (str_detect(name, "txt") == 0 ~map(value,set_names,new_names1),
                      str_detect(name, "txt") == 1 ~map(value,set_names,new_names2))) %>% 
  mutate(value = map(value, ~clean_C7(.x)))

## map C7toC5 df
corp_dat %<>% mutate(value = map(value,left_join,C7toC5))



# Create nested columns for text, C7 tags and C5 tags
corp_dat <- corp_dat %>% #slice(1) %>% 
  mutate(text = map(.x = value, .f = ~.x %>% 
                      group_by(Text_id) %>% 
                      summarise(text = paste(Token, collapse = " ")))) %>% 
  mutate(C5_pos = map(.x = value, .f = ~.x %>% 
                    group_by(Text_id) %>% 
                    summarise(C5 = paste(C5, collapse = " ")))) %>%
  mutate(C7_pos = map(.x = value, .f = ~.x %>% 
                    group_by(Text_id) %>% 
                    summarise(C7 = paste(clean_C7, collapse = " "))))

## check to see if there are any NAs the non-matches which are NA in C5 column
corp_dat  %>% mutate(NAs = map(.x = value, .f = ~.x %>% filter(is.na(C5)))) %>% select(NAs) %>% unnest(NAs) %>% 
 # filter(!str_detect(Text_id,"_")) %>% 
  count(C7, sort = TRUE)

## no NAs!


# check with BNC files ----------------------------------------------------
## the dat version is a nested df, good for purrr
bnc_dat <- readRDS(here::here("BNC_clean","data","bnc_dat.rds"))


## the df version is a tibble, easy to check
bnc_df <-  readRDS(here::here("BNC_clean","data","bnc_df.rds"))

## create a column with C5 only tags
bnc_df %<>% 
  mutate(C5 = map_chr(tags, ~str_replace_all(.x, "[^_]+_([^_]+)_", "\\1 "))) %>% 
  mutate(id = toupper(id))

bnc_df %>% count(id)

qwe %>% select(C5_pos) %>% unnest(c(C5_pos)) %>% 
  transmute(id = toupper(Text_id), C5_wmatrix = C5) %>% 
  inner_join(bnc_df)
