library(rvest)
library(tidyverse)
library(rio)
library(magrittr)

url <- "http://ucrel.lancs.ac.uk/claws/mapC7toC5.txt"

C7toC5 <- rio::import(url, header = F, quote="" )
names(C7toC5) <- c("C7","C5")
glimpse(C7toC5)

dat <- "/Users/timmarchand/Library/Mobile Documents/com~apple~CloudDocs/Wmatrix/csv_output/jusoc.txt_result.csv"
joc_wm <- read_csv(dat)


## purrr way to load multiple csv files as dataframes¥
## get file path of folder
file_path <- "/Users/timmarchand/Library/Mobile Documents/com~apple~CloudDocs/Wmatrix/csv_output/"

## assign files to variable
file_path %>% list.files() -> csv_file_names

csv_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    assign(x = str_remove(file_name, ".csv"), # Remove file extension ".csv"
           value = read_csv(paste0(file_path, file_name)),
           envir = .GlobalEnv) ## assign attributes the result to the Global Environment
  })


# Load everything into the Global Environment
csv_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    
    read_csv(paste0(file_path, file_name))
    
  }) -> df_list_read2

# assign file names as names for each part of the list
df_list_read2 <- set_names(df_list_read2 , str_remove(csv_file_names,"_result.csv"))

# change the df colnames

corp_dat <- df_list_read2 %>% enframe

new_names1 <- c("Text_id","Sentence","Count","C7","Token","Lemma")
new_names2 <- c("Text_id","Sentence","Count","Token","C7","Lemma")

corp_dat %<>% mutate(value = case_when(str_detect(name, "txt") == 0 ~map(value,set_names,new_names1),
                                      str_detect(name, "txt") == 1 ~map(value,set_names,new_names2)))

corp_dat %<>% mutate(value = map(value,left_join,C7toC5))
str(corp_dat)
