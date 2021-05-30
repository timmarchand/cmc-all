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



C7_df %>% transmute(unipos = C7_df$C7[C7_df$lemma != "PUNC",])

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



# remove C7_df as it is heavy
rm(C7_df)

C7_ngrams %>% count(tagcount) %>% ggplot(aes(n)) + 
  geom_histogram() + scale_x_continuous(trans = "log10" )

 


C7_counts <- C7_ngrams %>% distinct(corpus,id,tagcount)




## functions to pass through C7_ngrams

## get_freq_totals weights the posgrams towards longer texts in each corpus - therefore, wordcount not important
get_freq_totals <- function(data,num,total, wc = 0){
   ps <- sym(paste0(num,"pos"))
   cnt <- sym(paste0(num,"count"))
  data %>%
  filter(tagcount > wc) %>% 
  distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
  #add freq column
    mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
    group_by(corpus, {{ps}}) %>%
    summarise(sum = sum({{cnt}})) %>%
    slice_max(sum, n = total) %>%
    ungroup %>%
    distinct({{ps}}) %>% 
      rename_with(.,~glue("topSums_{total}_{ps}_{wc}"))
}


## get_freq_means gives each text equal standing, will be influenced by shorter texts - pay attention to wordcount to reduce the noise generated by shorter texts

get_freq_means <- function(data,num,total, wc = 0){
   ps <- sym(paste0(num,"pos"))
   cnt <- sym(paste0(num,"count"))
  data %>%
  filter(tagcount > wc) %>% 
  distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
  #add freq column
    mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
    group_by(corpus, {{ps}}) %>% 
    summarise(corpus_mean = mean(pos_freq))} %>% 
    slice_max(corpus_mean, n = total) %>%
    ungroup %>%
    distinct({{ps}}) %>% 
  pull %>% 
  assign(glue("topMeans_{total}_{ps}_{wc}"), .,envir = .GlobalEnv)
}

get_means <- function(data,pos){
  ps <- sym(paste0(pos,"pos"))
   cnt <- sym(paste0(pos,"count"))
   data %>% 
     distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
  #add freq column
    mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
    group_by(corpus, {{ps}}) %>% 
    summarise(corpus_mean = mean(pos_freq))}

C7_unigrams <- C7_ngrams %>% select(corpus, id, tagcount, unipos,unicount) %>% filter(!is.na(unipos)) %>% 
  mutate(pos_freq = unicount*1000/tagcount) %>% 
  mutate(type = "uni", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = unipos, count = unicount, pos_freq)


C7_bigrams <- C7_ngrams %>% select(corpus, id, tagcount, bipos,bicount) %>%   filter(!str_detect(bipos, "_NA|^NA")) %>% 
mutate(pos_freq = bicount*1000/tagcount) %>% 
  mutate(type = "bi", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = bipos, count = bicount, pos_freq)

C7_trigrams <- C7_ngrams %>% 
  select(corpus, id, tagcount, tripos,tricount) %>% 
  filter(!str_detect(tripos, "_NA|^NA")) %>% 
  mutate(pos_freq = tricount*1000/tagcount) %>% 
  mutate(type = "tri", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = tripos, count = tricount, pos_freq)

## use datatable to speed things up
l = list(C7_unigrams,C7_bigrams,C7_trigrams)
C7_dt <- rbindlist(l) 
### convert back  to a tibble from a datatable
setattr(C7_dt, "class", c("tbl", "tbl_df", "data.frame"))
## convert to a lazy_dt
setattr(C7_dt, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))

C7_dt[tagcount > 200]
C7_dt_200 <- C7_dt %>% filter(tagcount > 200)
C7_dt_200 <- C7_dt[tagcount > 200]
C7_dt_400 <- C7_dt[tagcount > 400]
C7_dt_600 <- C7_dt[tagcount > 600]
C7_dt_800 <- C7_dt[tagcount > 800]
C7_dt_1000 <- C7_dt[tagcount > 1000]

C7_dt_1000 %>% class

setattr(list(C7_dt_200,C7_dt_400,C7_dt_600,C7_dt_800,C7_dt_1000), "class", c("tbl", "tbl_df", "data.frame"))
C7_dt_1000 %>% class



C7_freqs <- bind_rows(C7_unigrams,C7_bigrams,C7_trigrams) 

nested_freqs <-  nest(C7_freqs, data =(everything())) %>% 
  slice(rep(1, each=6)) %>% 
  # add threshold column
  mutate(threshold = c(seq(0,1000,200))) 

qwe <- 
nested_freqs %>% 
  mutate(data = map2(.x = data, 
                     .y = threshold, 
                     .f = ~ nest_filter(.x,.y))) 

nest_filter <- function(x,y){
  x %>% filter(tagcount >= y)
}

 mutate(data = map2(.x = data, 
                     .y = threshold, 
                     .f = ~ nest_filter(.x,.y)))


C7_trigrams %>% mutate(type = "tri", .after =id) %>% 
  select(corpus,id, tagcount, type, pos = tripos, count = tricount, pos_freq)

get_wt_means <- function(data,pos){
  ps <- sym(paste0(pos,"pos"))
   cnt <- sym(paste0(pos,"count"))
   data %>% 
     distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
  #add freq column
    mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
    group_by(corpus, {{ps}}) %>% 
    summarise(corpus_wt.mean = weighted.mean(pos_freq,tagcount))}

get_totals <- function(data,pos){
  ps <- sym(paste0(pos,"pos"))
   cnt <- sym(paste0(pos,"count"))
   data %>% 
     distinct(corpus, id, {{ps}}, {{cnt}}, tagcount ) %>%
  #add freq column
    mutate(pos_freq = {{cnt}}*1000 / tagcount) %>%
    group_by(corpus, {{ps}}) %>% 
    summarise(corpus_sum = sum(pos_freq)/sum(tagcount))}

C7_ngrams %>% 
  get_means("uni") %>% arrange(-corpus_mean)

C7_ngrams %>% 
  get_totals("uni") %>% arrange(-corpus_sum)

C7_ngrams %>% 
  get_wt_means("uni") %>% arrange(-corpus_wt.mean)


## create references for later analysis
unigram_match <- C7_ngrams %>% distinct(unipos,unigram)
bigram_match <- C7_ngrams %>% distinct(bipos,bigram)
trigram_match <- C7_ngrams %>% distinct(tripos,trigram)



trigram_match %>% filter(str_detect(tripos, topMeans_40_tripos_600)) %>% arrange(tripos)

get_top <- function(data){
data %>% group_by(corpus,type,pos) %>% summarise(mean_freq = mean(pos_freq)) %>% ungroup %>% 
  group_by(corpus, type) %>% 
  slice_max(mean_freq, n=12)
}

get_top_n <- function(data,n){
data %>% 
mutate(type = factor(type, levels = c("uni","bi","tri"))) %>% 
group_by(corpus,type,pos) %>% 
summarise(mean_freq = mean(pos_freq)) %>% 
 ungroup %>% 
group_by(corpus, type) %>%
slice_max(mean_freq, n=n) %>%
    ungroup
}
C7_dt_1000 %>% get_top_n(n = 12)
setattr(C7_dt_1000, "class", c("tbl", "tbl_df", "data.frame"))

C7_dt_800 %>% 
group_by(corpus,type,pos) %>% 
summarise(mean_freq = mean(pos_freq)) %>% 
 ungroup %>% 
group_by(corpus, type) %>%
slice_max(mean_freq, n=n)

DT800 <- C7_dt_800[,.(mean_freq = mean(pos_freq)), keyby = .(corpus, type, 
    pos)][
      order(-mean_freq), head(.SD, 12), by = .(corpus,type)]


DT[!duplicated(DT[2:3]),]

DT[,.("type","pos")]
unique(DT, by = c("type", "pos"))


get_topn_wt <- function(data,n){
data %>% 
mutate(type = factor(type, levels = c("uni","bi","tri"))) %>% 
group_by(corpus,type,pos) %>% 
summarise(wt.mean_freq = weighted.mean(pos_freq, wt = tagcount)) %>% 
ungroup %>% 
group_by(corpus, type) %>% 
slice_max(wt.mean_freq, n=n) %>% 
    ungroup
}


nested_tops <- nested_freqs %>% 
  mutate(top12 = map(data, ~get_topx(.x, x = 12))) %>% 
  mutate(top25 = map(data, ~get_topx(.x, x = 25))) %>% 
  mutate(top50 = map(data, ~get_topx(.x, x = 50))) %>% 
  mutate(top100 = map(data, ~get_topx(.x, x = 100)))

qwe <- nested_freqs %>% 
  mutate(top25wt = map(data, ~get_topn_wt(.x, n = 25)))

pull_ngrams <- function(data){
data  %>% 
  # mutate(type = factor(type, levels = c("uni","bi","tri"))) %>% 
  ungroup %>% 
  distinct(type,pos) %>% 
  pull
}

nested_tops %>% slice(1) %>% 
  mutate(ngrams = map(.x = top12, .f = ~pull_ngrams(.x))) %>% 
  mutate(filtered = map(.x = data, ~filter(.x %in% ngrams)))
  filter(pos %in% pluck("top12",1) %>% pull_ngrams

C7_tbl <- as_tibble(C7_dt)


C7_tbl %>% filter(type == "bi", tagcount > 600 ) %>% 
  # select(id,pos,pos_freq) %>% 
  pivot_wider(names_from = pos, values_from = pos_freq, values_fill = 0)


qwe %>% pluck("top25wt",2) %>% 
  mutate(type = factor(type, levels = c("uni","bi","tri"))) %>% 
  ungroup %>% 
  distinct(type,pos) %>% 
  filter(type == "tri") -> test2

asd %>% group_by(corpus, type) %>% slice_max(mean_freq, n=12) %>% filter(type == "bi") %>% count(corpus)

qwe(data = C7_ngrams,num = "tri")

asd <- get_freq_totals(C7_ngrams, "tri", 40,600) 
get_freq_means(C7_ngrams, "tri", 40,600)


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

