library(rvest)
library(rio)
library(magrittr)
library(tidylog)
library(tidytext)
library(tidyverse)



# functions ---------------------------------------------------------------
## this function takes the wmatrix C7 tags, and removes the markers of pormanteau tags to create a clean_C7 value

## it then further simplifies the C7 tags to allow the C5 tags to be matched when joined

clean_C7toC5 <-  function(df){
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

clean_C7 <-  function(df){
  df %>% mutate(C7 = str_remove(C7, "@"),
                C7 = str_remove(C7, "%")) 
}

`%notin%` <- Negate(`%in%`)


# data --------------------------------------------------------------------
## CLAWS tags etc

bnc_df <- readRDS("~/Desktop/cmc-all/BNC_clean/data/bnc_df.rds")
## get the mapping of C& to C5 tags from wmatrix and import
url <- "http://ucrel.lancs.ac.uk/claws/mapC7toC5.txt"
C7toC5 <- rio::import(url, header = F, quote="" )
## add colnames to match the later join
names(C7toC5) <- c("alt_C7","C5")

##
page <- "http://ucrel.lancs.ac.uk/claws7tags.html"
C7tagset <- rio::import(page, header = TRUE)
names(C7tagset) <- c("C7","definition")
C7tagset %<>% mutate(C7 = str_squish(C7),
                    definition = str_squish(definition))

page <- "http://ucrel.lancs.ac.uk/claws5tags.html"
C5tagset <- rio::import(page, header = TRUE)
names(C5tagset) <- c("C5","definition")
C5tagset %<>% mutate(C5 = str_squish(C5),
                     definition = str_squish(definition))

#remove punctuation tags for wordcounting
C5tagwc <- C5tagset %>% filter(C5 %notin% c("PUL","PUR","PUN","PUQ"))


## purrr way to load multiple csv files as dataframes
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
  )
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
bnc_df <-  

## create a column with C5 only tags
# bnc_df %<>% 
#   mutate(C5 = map_chr(tags, ~str_replace_all(.x, "[^_]+_([^_]+)_", "\\1 "))) %>% 
#   mutate(id = toupper(id))

## pivot long the bnc_df

bnc_df <- 
  readRDS(here::here("BNC_clean","data","bnc_df.rds")) %>% 
  # choose columns of interest
  select(id,tags) %>% 
  # unnest tags specifying token as spaces to capture all punctuations
  unnest_tokens(output = word, input = tags, to_lower = FALSE,token = stringr::str_split, pattern = " " ) %>% 
 # create columns for word, C5 and lemma
   separate(word, into = c("word","C5","lemma"), sep ="_") %>% 
# add corpus column  generated from id
  separate(id, c("corpus","id"), sep ="_", extra = "merge")


cmc_df <- readRDS(here::here("wmatrix","data","wmatrix_dat.rds")) %>% 
  # choose only the two cmc corpus
  filter(corpus %in% c("hysoc","jusoc")) %>% 
  # rename pos to C7 for later join
  mutate(data = map(data, .f = ~ .x %>% rename(C7 = pos))) %>% 
  # clean the C7 tags, creating a alt one for the join and a clean one
  mutate(data = map(data, ~clean_C7toC5(.x))) %>% 
  # left_join to map C5 tags over
  mutate(data = map(data,left_join,C7toC5)) %>% 
  # unnest to create a df
  unnest(c(data)) %>% 
  # choose columns of interest
  select(corpus,id,word,C5, lemma)

cmc_df
bnc_df

C5_df <- bind_rows(bnc_df,cmc_df) %>% 
  # remove portmanteau tags
  mutate(C5 = str_remove(C5,"-.+")) %>% 
  # standardise the corpus names
  mutate(corpus = case_when(
    corpus =="hysoc" ~ "HOC",
    corpus =="dem"   ~ "DEM",
    corpus =="fic"   ~ "FIC",
    corpus =="aca"   ~ "ACA",
    corpus =="news"   ~ "NWS",
    corpus =="jusoc"  ~ "JOC"))  %>%
  # add corpus identifier to the front of each id
  mutate(id = paste0(corpus,"_",id))

saveRDS(C5_df, here::here("wmatrix","data","C5_df.rds"))
 



C7_df <- readRDS(here::here("wmatrix","data","wmatrix_dat.rds")) %>% 
  # rename pos to C7 for later join
  mutate(data = map(data, .f = ~ .x %>% rename(C7 = pos))) %>% 
  # clean the C7 tags, creating a alt one for the join and a clean one
  mutate(data = map(data, ~clean_C7(.x))) %>% 
  unnest(c(data)) %>% 
  select(-count) %>% 
  # standardise the corpus names
  mutate(corpus = case_when(
    corpus =="hysoc" ~ "HOC",
    corpus =="dem"   ~ "DEM",
    corpus =="fic"   ~ "FIC",
    corpus =="aca"   ~ "ACA",
    corpus =="news"   ~ "NWS",
    corpus =="jusoc"  ~ "JOC")) %>% 
# add corpus identifier to the front of each id
mutate(id = case_when(
  corpus == "HOC" ~ paste0(corpus,"_",id),
  corpus == "JOC" ~ paste0(corpus,"_",id),
  corpus == "NWS" ~ str_replace(id, "NEWS","NWS"),
  TRUE ~ id))
  

## save the base version to data folder (38MB)
saveRDS(C7_df, here::here("wmatrix","data","C7_df.rds"))



# generate ngrmas ---------------------------------------------------------
C5_df <- readRDS(here::here("wmatrix","data","C5_df.rds"))
C5_df %>% filter(corpus == "JOC" | corpus == "HOC")


C7_df <- readRDS(here::here("wmatrix","data","C7_df.rds"))





C7_ngrams <- C7_df %>% 
  # filter out posgrams overlapping punctuation
  filter(!str_detect(lemma, "PUNC")) %>% 
  # add unipos for checking
  mutate(unipos = C7) %>% 
  # create bigram tags, matching with the next pos down
  mutate(bipos = paste(C7,lead(C7,1),sep ="_")) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(tripos = paste(C7,lead(C7,1), lead(C7,2), sep ="_")) %>% 
 # to lower for unigrams
  mutate(unigram = tolower(word)) %>%
  # create bigram tags, matching the next pos down
  mutate(bigram = paste(word,lead(word,1),sep ="_") %>% tolower) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(trigram = paste(word,lead(word,1), lead(word,2), sep ="_") %>% tolower) %>%
  ## add count based on the number of tags analysed
  add_count(id, name = "tagcount") %>% 
  add_count(id,unipos, name = "uni_count") %>% 
  add_count(id, bipos, name = "bi_count") %>% 
  add_count(id, tripos, name = "tri_count")
  

C7_ngrams %>% filter(tagcount == uni_count)

 



C7_counts <- C7_ngrams %>% distinct(corpus,id,tagcount)

## nest the df to add bipos and tripos counts quickly
C7_nest <- 
C7_df %>% nest(data = id:tagcount) %>% 
  mutate(data = map(data, ~.x %>% 
              add_count(id, bipos, name = "bipos_count"))) %>%
  mutate(data = map(data, ~.x %>% 
                  add_count(id, tripos, name = "tripos_count")))

C7_freq <- 
C7_ngrams %>% distinct(corpus, id, bipos, bi_count, tagcount ) %>%
  #add freq column
  mutate(bipos_freq = bi_count*1000 / tagcount)

## set the top bipos for each corpus
C7_freq %>% 
  group_by(corpus,bipos) %>% 
  summarise(total = sum(bi_count)) %>% 
  dplyr::slice_max(total,n = 50) %>% 
  ungroup %>% 
  distinct(bipos)

## function to pass through C7_ngrams
get_totals <- function(x,total,pos,count){
  x %>%
   distinct(corpus, id, {{pos}}, {{count}}, tagcount ) %>%
    #add freq column
    mutate(pos_freq = {{count}}*1000 / tagcount) %>% 
    group_by(corpus, {{pos}}) %>% 
    summarise(sum = sum({{count}})) %>% 
    slice_max(sum, n = total) %>% 
    ungroup %>% 
    distinct({{pos}})
}

top_50_trigrams <- get_totals(C7_ngrams, 50, tripos,tri_count)
top_50_bigrams <- get_totals(C7_ngrams, 50, biipos,bi_count)
top_12_trigrams <- get_totals(C7_ngrams, 12, tripos,tri_count)

  # pivot wider
  pivot_wider(id_cols = c(corpus,id), names_from = bipos, 
              values_from = bipos_freq, values_fill = 0)


C5_df %<>% 
  # create bigram tags, matching the next pos down
  mutate(bipos = paste(C5,lead(C5,1),sep ="_")) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(tripos = paste(C5,lead(C5,1), lead(C5,2), sep ="_")) %>% 
  # create bigram tags, matching the next pos down
  mutate(bigram = paste(word,lead(word,1),sep ="_") %>% tolower) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(trigram = paste(word,lead(word,1), lead(word,2), sep ="_") %>% tolower) %>% 
  # filter out posgrams overlapping punctuation
  filter(!str_detect(lemma, "[,\\.:;\\?!]"))

C7_df %>% count(corpus, sort = TRUE)
C5_df %>% count(id, sort = TRUE)

bnc_df %>% select(id) %>% mutate(id = toupper(id)) #%>% inner_join(C7_n)

## add C7 wordcounts by semi_join with tagset
C7_wc <- 
  C7_df %>% semi_join(C7tagset) %>% add_count(id, name = "wc") %>% distinct(id,wc)

C5_wc <- 
  C5_df %>% semi_join(C5tagwc) %>% add_count(id, name = "wc") %>% distinct(id,wc)


C7tagset %>% pull(C7)

