library(rvest)
library(rio)
library(magrittr)
library(tidylog)
library(tidytext)
library(tidyverse)

pacman::p_load(rio,here,magrittr,glue,tidytext,tidyverse,tidylog, data.table,dtplyr)


n
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
names(C7toC5) <- c("unipos","C5")

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

## group cmc corpora into threads
JOC_blend <- 
  C7_df %>% filter(corpus == "JOC") %>%
  separate(id, c("id","comment"),"(?<=[A-Z])(?=[0-9])") %>% 
  select(-comment)

HOC_blend <-
  C7_df %>% filter(corpus == "HOC") %>% 
  separate(id, c("id","comment"),"(?<=[A-Z])(?=[0-9])") %>% 
  select(-comment)

rm(HOC_blend)
rm(JOC_blend)

# bind with C7_df
C7_df %<>% filter(corpus != "HOC", corpus != "JOC") %>% 
  bind_rows(JOC_blend) %>% 
  bind_rows(HOC_blend)
  

## save the base version to data folder (38MB)
saveRDS(C7_df, here::here("wmatrix","data","C7_df.rds"))



# generate ngrmas ---------------------------------------------------------
C5_df <- readRDS(here::here("wmatrix","data","C5_df.rds"))
C5_df %>% filter(corpus == "JOC" | corpus == "HOC")


C7_df <- readRDS(here::here("wmatrix","data","C7_df.rds"))



C7_ngrams <- C7_df %>% 
  # filter out posgrams overlapping punctuation
  # filter(!str_detect(lemma, "PUNC")) %>% 
  # add unipos for checking
  mutate(unipos = ifelse(lemma == "PUNC", NA,C7)) %>% 
  # create bigram tags, matching with the next pos down
  mutate(bipos = paste(unipos,lead(unipos,1),sep ="_")) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(tripos = paste(unipos,lead(unipos,1), lead(unipos,2), sep ="_")) %>% 
 # to lower for unigrams
  mutate(unigram = tolower(word)) %>%
  # create bigram tags, matching the next pos down
  mutate(bigram = paste(word,lead(word,1),sep ="_") %>% tolower) %>% 
  # create trigram tags, matching the next 2 pos down
  mutate(trigram = paste(word,lead(word,1), lead(word,2), sep ="_") %>% tolower) %>%
   group_by(id) %>% 
  ## add count based on the number of tags analysed
  mutate(tagcount = sum(!is.na(unipos))) %>% 
  ungroup %>% 
  group_by(id,unipos) %>% 
  mutate(unicount = sum(!is.na(unipos))) %>% 
  ungroup %>% 
  group_by(id,bipos) %>% 
  mutate(bicount = sum(!str_detect(bipos,"^NA|_NA"))) %>% 
  ungroup %>% 
  group_by(id,tripos) %>% 
  mutate(tricount = sum(!str_detect(tripos,"^NA|_NA"))) %>% 
  ungroup

saveRDS(C7_ngrams, here::here("wmatrix","data","C7_ngrams.rds"))

 C5_ngrams <- C7_df %>% 
  # filter out posgrams overlapping punctuation
  # filter(!str_detect(lemma, "PUNC")) %>% 
  # add unipos for checking
  mutate(unipos = ifelse(lemma == "PUNC", NA,C7)) %>% 
   # left join on unipos
  left_join(C7toC5) %>% 
  # change NA C5 that match unipos ditto tags
  mutate(C5 = ifelse(is.na(C5) &!is.na(unipos), unipos, C5)) %>% 
  select(corpus,id,C5,word,lemma) %>% 
   mutate(unipos = ifelse(lemma == "PUNC", NA,C5)) %>% 
   # create bigram tags, matching with the next pos down
   mutate(bipos = paste(unipos,lead(unipos,1),sep ="_")) %>% 
   # create trigram tags, matching the next 2 pos down
   mutate(tripos = paste(unipos,lead(unipos,1), lead(unipos,2), sep ="_")) %>% 
   # to lower for unigrams
   mutate(unigram = tolower(word)) %>%
   # create bigram tags, matching the next pos down
   mutate(bigram = paste(word,lead(word,1),sep ="_") %>% tolower) %>%
   # create trigram tags, matching the next 2 pos down
   mutate(trigram = paste(word,lead(word,1), lead(word,2), sep ="_") %>% tolower) %>%
   group_by(id) %>% 
   ## add count based on the number of tags analysed
   mutate(tagcount = sum(!is.na(unipos))) %>% 
   ungroup %>% 
   group_by(id,unipos) %>% 
   mutate(unicount = sum(!is.na(unipos))) %>% 
   ungroup %>% 
   group_by(id,bipos) %>% 
   mutate(bicount = sum(!str_detect(bipos,"^NA|_NA"))) %>% 
   ungroup %>% 
   group_by(id,tripos) %>% 
   mutate(tricount = sum(!str_detect(tripos,"^NA|_NA"))) %>% 
   ungroup
  
 
 saveRDS(C5_ngrams, here::here("wmatrix","data","C5_ngrams.rds"))


# read in C7 or C5 ngrams -------------------------------------------------

C7_ngrams <- readRDS((here::here("wmatrix","data","C7_ngrams.rds")))
C5_ngrams <- readRDS((here::here("wmatrix","data","C5_ngrams.rds")))
# C7_ngrams %>% count(tagcount) %>% ggplot(aes(n)) + 
#   geom_histogram() + scale_x_continuous(trans = "log10" )
# 


C7_unigrams <- C7_ngrams %>% select(corpus, id, tagcount, unipos,unicount) %>%  filter(!str_detect(unipos, "[.]{3}")) %>% 
  filter(!is.na(unipos)) %>% 
  mutate(pos_freq = unicount*1000/tagcount) %>% 
  mutate(type = "uni", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = unipos, count = unicount, pos_freq)


C7_bigrams <- C7_ngrams %>% select(corpus, id, tagcount, bipos,bicount) %>%   filter(!str_detect(bipos, "_NA|^NA")) %>% 
  filter(!str_detect(bipos, "[.]{3}")) %>% 
mutate(pos_freq = bicount*1000/tagcount) %>% 
  mutate(type = "bi", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = bipos, count = bicount, pos_freq)

C7_trigrams <- C7_ngrams %>% 
  select(corpus, id, tagcount, tripos,tricount) %>% 
  filter(!str_detect(tripos, "_NA|^NA")) %>% 
  filter(!str_detect(tripos, "[.]{3}")) %>% 
  mutate(pos_freq = tricount*1000/tagcount) %>% 
  mutate(type = "tri", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = tripos, count = tricount, pos_freq)

## use datatable to speed things up
l = list(C7_unigrams,C7_bigrams,C7_trigrams)
C7_dt <- rbindlist(l) 
### convert back  to a tibble from a datatable
setattr(C7_dt, "class", c("tbl", "tbl_df", "data.frame"))
## btw convert to a lazy_dt
# setattr(C7_dt, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))



C5_unigrams <- C7_ngrams %>% select(corpus, id, tagcount, unipos,unicount) %>% filter(!is.na(unipos)) %>% 
  filter(!str_detect(unipos, "[.]{3}")) %>% 
  mutate(pos_freq = unicount*1000/tagcount) %>% 
  mutate(type = "uni", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = unipos, count = unicount, pos_freq)


C5_bigrams <- C7_ngrams %>% select(corpus, id, tagcount, bipos,bicount) %>%   filter(!str_detect(bipos, "_NA|^NA")) %>% 
  filter(!str_detect(bipos, "[.]{3}")) %>% 
  mutate(pos_freq = bicount*1000/tagcount) %>% 
  mutate(type = "bi", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = bipos, count = bicount, pos_freq)

C5_trigrams <- C7_ngrams %>% 
  select(corpus, id, tagcount, tripos,tricount) %>% 
  filter(!str_detect(tripos, "_NA|^NA")) %>% 
  filter(!str_detect(tripos, "[.]{3}")) %>% 
  mutate(pos_freq = tricount*1000/tagcount) %>% 
  mutate(type = "tri", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = tripos, count = tricount, pos_freq)



## functions to pass through C7_ngrams

## get_freq_totals weights the posgrams towards longer texts in each corpus - therefore, wordcount not important
# get_freq_totals <- function(data,num,total, wc = 0){
#    ps <- sym(paste0(num,"pos"))
#    cnt <- sym(paste0(num,"count"))
#   data %>%
#   filter(tagcount > wc) %>% 
#   distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
#   #add freq column
#     mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
#     group_by(corpus, {{ps}}) %>%
#     summarise(sum = sum({{cnt}})) %>%
#     slice_max(sum, n = total) %>%
#     ungroup %>%
#     distinct({{ps}}) %>% 
#       rename_with(.,~glue("topSums_{total}_{ps}_{wc}"))
# }


## get_freq_means gives each text equal standing, will be influenced by shorter texts - pay attention to wordcount to reduce the noise generated by shorter texts

# get_freq_means <- function(data,num,total, wc = 0){
#    ps <- sym(paste0(num,"pos"))
#    cnt <- sym(paste0(num,"count"))
#   data %>%
#   filter(tagcount > wc) %>% 
#   distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
#   #add freq column
#     mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
#     group_by(corpus, {{ps}}) %>% 
#     summarise(corpus_mean = mean(pos_freq))} %>% 
#     slice_max(corpus_mean, n = total) %>%
#     ungroup %>%
#     distinct({{ps}}) %>% 
#   pull %>% 
#   assign(glue("topMeans_{total}_{ps}_{wc}"), .,envir = .GlobalEnv)
# }
# 
# get_means <- function(data,pos){
#   ps <- sym(paste0(pos,"pos"))
#    cnt <- sym(paste0(pos,"count"))
#    data %>% 
#      distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
#   #add freq column
#     mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
#     group_by(corpus, {{ps}}) %>% 
#     summarise(corpus_mean = mean(pos_freq))}

# 
# C7_freqs <- bind_rows(C7_unigrams,C7_bigrams,C7_trigrams) 
# 
# nested_freqs <-  nest(C7_freqs, data =(everything())) %>% 
#   slice(rep(1, each=6)) %>% 
#   # add threshold column
#   mutate(threshold = c(seq(0,1000,200))) 
# 
# qwe <- 
# nested_freqs %>% 
#   mutate(data = map2(.x = data, 
#                      .y = threshold, 
#                      .f = ~ nest_filter(.x,.y))) 
# 
# nest_filter <- function(x,y){
#   x %>% filter(tagcount >= y)
# }
# 
#  mutate(data = map2(.x = data, 
#                      .y = threshold, 
#                      .f = ~ nest_filter(.x,.y)))
# 
# 
# C7_trigrams %>% mutate(type = "tri", .after =id) %>% 
#   select(corpus,id, tagcount, type, pos = tripos, count = tricount, pos_freq)
# 
# get_wt_means <- function(data,pos){
#   ps <- sym(paste0(pos,"pos"))
#    cnt <- sym(paste0(pos,"count"))
#    data %>% 
#      distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
#   #add freq column
#     mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
#     group_by(corpus, {{ps}}) %>% 
#     summarise(corpus_wt.mean = weighted.mean(pos_freq,tagcount))}
# 
# get_totals <- function(data,pos){
#   ps <- sym(paste0(pos,"pos"))
#    cnt <- sym(paste0(pos,"count"))
#    data %>% 
#      distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
#   #add freq column
#     mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
#     group_by(corpus, {{ps}}) %>% 
#     summarise(corpus_sum = sum(pos_freq)/sum(tagcount))}
# 
# C7_ngrams %>% 
#   get_means("uni") %>% arrange(-corpus_mean)
# 
# C7_ngrams %>% 
#   get_totals("uni") %>% arrange(-corpus_sum)
# 
# C7_ngrams %>% 
#   get_wt_means("uni") %>% arrange(-corpus_wt.mean)


## create references for later analysis
unigram_match <- C7_ngrams %>% distinct(unipos,unigram)
bigram_match <- C7_ngrams %>% distinct(bipos,bigram)
trigram_match <- C7_ngrams %>% distinct(tripos,trigram)



# trigram_match %>% filter(str_detect(tripos, topMeans_40_tripos_600)) %>% arrange(tripos)
# 
# get_top <- function(data){
# data %>% group_by(corpus,type,pos) %>% summarise(mean_freq = mean(pos_freq)) %>% ungroup %>% 
#   group_by(corpus, type) %>% 
#   slice_max(mean_freq, n=12)
# }

# function to generate top ngrams by certain criteria

get_top_n <- function(data,n,cut = 0, type = "all"){
type <- ifelse(type == "all", "uni|bi|tri", type)
  df <- data %>% 
    filter(type == sym({{type}})) %>% 
    filter(tagcount > cut) %>% 
group_by(corpus,type,pos) %>% 
summarise(mean_freq = mean(pos_freq)) %>% 
 ungroup %>% 
group_by(corpus, type) %>%
slice_max(mean_freq, n=n) %>%
    ungroup
  #type <- ifelse(type == "uni|bi|tri","all , type)
  ## get names of the arguments
  name <- sym(glue::glue("top{n}_{type}gram_{cut}"))
  l <- list(df)
  # name each list element by its arguments
 l <-  set_names(l,name)
  return(l)
}

get_top_n <- function(data,n,cut = 0, type){
  df <- data %>% 
    filter(type == sym({{type}})) %>%
    filter(tagcount > cut) %>% 
    group_by(corpus,type,pos) %>% 
    summarise(mean_freq = mean(pos_freq)) %>% 
    ungroup %>% 
    group_by(corpus, type) %>%
    slice_max(mean_freq, n=n) %>%
    ungroup
  ## get names of the arguments
  name <- sym(glue::glue("top{n}_{type}gram_{cut}"))
  l <- list(df)
  # name each list element by its arguments
  l <-  set_names(l,name)
  return(l)
}



## set the values for pmap
ns <- c(20,35,50)
cuts <- seq(0,1000,500)
types <- c("uni","bi","tri")

## get every combination of them in a df for settings
vals<- crossing(types,ns,cuts)

## use pmap on the vals, feed it into the function
## use {..n} to specify whcih column to use
C7_tops <- pmap(vals, ~get_top_n(data = C7_dt, n = {..2}, cut = {..3}, type = {..1})) %>% 
## flatten the list by one level
flatten %>% 
## enframe to created a nested tibble
enframe(name = "setting", value = "data")

## create  columns of all the selected posgrams as strings
C7_tops %<>% mutate(strings = map(data, ~.x %>% pull(pos) %>% unique))


## create a df with 
## the difference with C7_unigrams is that the former filters out punctuation and retain information for posgram selection
## this one retains all tags and word information as a df including ngrams renamed as ngrams
## 
C7_uni <- C7_ngrams %>% select(c(1,2,4,5,starts_with("uni"),"tagcount")) %>% 
  mutate(pos_freq = unicount*1000/tagcount) %>% 
  rename(pos = unipos, token = unigram, count = unicount) %>% 
  mutate(type = "uni", .after = id)

C7_bi <- C7_ngrams %>% select(c(1,2,4,5,starts_with("bi"),"tagcount")) %>% 
  mutate(pos_freq = bicount*1000/tagcount) %>% 
  rename(pos = bipos, token = bigram, count = bicount)%>% 
  mutate(type = "bi", .after = id)

C7_tri <- C7_ngrams %>% select(c(1,2,4,5,starts_with("tri"), "tagcount")) %>% 
  mutate(pos_freq = tricount*1000/tagcount) %>%
  rename(pos = tripos, token = trigram, count = tricount) %>% 
  mutate(type = "tri", .after = id)

C7_full_df <- bind_rows(C7_tri,C7_bi,C7_uni)

saveRDS(C7_full_df, here::here("wmatrix","data","C7_full_df.rds"))

## separate settings into useful columns
C7_tops %<>% separate(setting,into = c("top","posgram","cut"),sep = "_",remove = FALSE) %>% mutate(cut = as.integer(cut))

# match the pos strings for each setting to subset the C7_ngrams df
C7_tops %<>% mutate(for_pivot = map2(.x= strings, .y = cut, .f = ~C7_full_df%>% filter(pos %in% .x) %>% filter(tagcount %in% .y)))


# use the for_pivot subset to pivot wider and create dataframes suitable for efa
C7_tops %<>% 
  mutate(for_efa = map(.x = for_pivot,
                      .f = ~.x %>% 
         # use distinct to remove duplicates
          distinct(corpus, id, type, pos, pos_freq) %>% 
          pivot_wider(names_from = pos, values_from = pos_freq, values_fill =0)))


saveRDS(C7_tops,here::here("wmatrix", "data","C7_tops.rds"))



library(scales)
library(ggeasy)



## finding out the number of factors
## pull out the suggeste number of factors using CNG and beta estimate from multiple regression

## functions to extract CNG and beta figures from n_factors dataframes
CNG_extract <- function(df){
  df %>% filter(Method == "CNG") %>% pull(n_Factors)
}

beta_extract  <- function(df){
  df %>% filter(Method == "beta") %>% pull(n_Factors)
}

master_details %>% arrange(-determinant)%>% # slice(1) %>% 
  transmute(CNG = map_dbl(.x = n_factors_df , .f = ~CNG_extract(.x))) %>% count(CNG)

master_details %>% arrange(-determinant)%>% # slice(1) %>% 
  transmute(beta = map_dbl(.x = n_factors_df , .f = ~beta_extract(.x))) %>% count(beta)

# either 3 or 4 factor solution suggested


# dat <- master_details %>% 
#   filter(top == 50) %>% 
#   arrange(-determinant) %>% 
#   pluck("cor_mat",1)
# 
# 
# dat
# 
# 
# nFactors::nCng(dat,model = "factors")
# nFactors::nSeScree(dat,model = "factors")
# nFactors::nScree(dat,model = "factors")
# results <- nFactors::nMreg(dat,model = "factors")
# 
# nFactors::plotuScree(dat, main=paste(results$nFactors[1], ", ",
#                                      results$nFactors[2], " or ",
#                                      results$nFactors[3],
#                                      " factors retained by the MREG procedures",
#                                      sep="")) 


## concerns about multicolinearity
## check the determinant of the matrix

library(scales)
library(ggeasy)

## range of determinants vs posgram
p1 <-  %>% 
  mutate(determinant = log10(determinant)) %>% 
  ggplot(aes(determinant, fill = posgram)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() +  easy_remove_y_axis() + easy_move_legend("top")

## range of determinants vs number of posgrams
p2 <- master_details %>% 
  mutate(determinant = log10(determinant)) %>% 
  mutate(top = factor(top)) %>% 
  ggplot(aes(determinant, fill = top)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() + easy_remove_y_axis() + easy_move_legend("top")

## range of determinants vs wordcount threshold
p3 <- master_details %>%
  mutate(determinant = log10(determinant)) %>% 
  mutate(threshold = factor(threshold)) %>% 
  ggplot(aes(determinant, fill = threshold)) + geom_boxplot() + scale_x_continuous(labels = scales::scientific) + geom_vline(aes(xintercept = log10(0.00001)), colour = "red", linetype = 2) + theme_minimal() + easy_remove_y_axis() + easy_move_legend("top")


gridExtra::grid.arrange(p1,p2,p3, nrow = 3)


## results ... try trigrams, top 25, 400 cut

dat <- master_details %>% filter(type == "trigram_400", top == 25) %>% 
  pluck("matrix", 1) 

dat %>% 
  check_factorstructure()

mdl1 <- fa(dat, nfactors=4, rotate = "oblimin", fm = "pa")

get_loadings(dat)

tbl <-  nest(dat,data = everything()) %>% 
  # duplicate data for maximum number of factors 
  uncount(2) %>% 
  # add factors column
  rowid_to_column(var = "factors") %>% 
  # create list column with fa results for each factor
  mutate(model = map2(.x = .$factors, .y = .$data, 
                      .f = ~fa(.y, nfactors = .x, 
                               rotate = "oblimin", fm = "pa"))) %>% 
  # add BIC information for each model
  mutate(BIC = map_dbl(.x = model, .f = ~get_BIC(.x))) %>% 
  # add loadings table
  mutate(loadings = map(.x = model, .f = ~get_loadings(.x, cut = 0))) %>% 
  # add factor correlations table
  mutate(fa_corrs = map(.x = model, .f = ~get_fa_cors(.x))) %>% 
  # get cfi scores
  mutate(cfi = map_dbl(.x = model, .f = ~get_cfi(.x))) #%>% 
mutate(RMSEA = map_dbl(.x = model, .f = ~pluck(1,"RMSEA")))




