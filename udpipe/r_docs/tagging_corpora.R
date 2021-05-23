
# Libraries ---------------------------------------------------------------
suppressMessages({
  library(here)
  library(tidyverse)
  # library(XML)
  # # library(methods)
  # library(rvest)
  # library(fs)
  # library(rlang)
  library(magrittr) # for the %<>% operator
  library(zip)
  library(dplyr)
  library(udpipe)
  library(readtext)
  library(tictoc)
  library(tidytext)   
  library(tidylog)
})

## BNC ----
# Paths -------------------------------------------------------------------
zipfile <- here("BNC_clean","data", "bnc_txt.zip")
tmp <- tempfile()
junk <- tempdir("junk")
out <- tempdir("out")


# Unzip zip to tmp
unzip(zipfile, exdir = tmp)




# procezsing from zip -----------------------------------------------------
## get file paths and names
list_paths <- list.files(path = tmp, pattern = ".txt", full.names = TRUE)
files <- list_paths %>% basename %>% path_ext_remove()

# Unzip -------------------------------------------------------------------


# Process -----------------------------------------------------------------


## for threads use readtext()  
threads <- tibble(file = files, path = list_paths) %>%
  mutate(
    text = map2(path, file, ~readtext(.x))) %>% 
  unnest(text) %>% 
  select(-c(path,doc_id))


## for comments use read_lines()  
comments <-   tibble(file = files, path = list_paths) %>%
  mutate(
    text = map2(path, file, ~read_lines(.x))) %>% select(-path) %>% 
  unnest(text) %>% 
  group_by(file) %>% mutate(file=paste0(file,"_",row_number() %>% str_pad(3,"left","0")))

## load tagging model      
mdl <- udpipe_load_model(file = here("BNC_clean", "data", "english-ewt-ud-2.5-191206.udpipe"))   


## getting full tags and tags only columns
bnc_threads <- threads %>%
  mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
  select(file,tagged) %>% 
  mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
  mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
  mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
  unnest(c(full_tags, tags_only))


bnc_comments <- comments %>%
  mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
  select(file,tagged) %>% 
  mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
  mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
  mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
  unnest(c(full_tags, tags_only))

## save as rdata

saveRDS(bnc_threads, file = here("ignored_files", "bnc_threads.rds"))
saveRDS(bnc_comments, file = here("ignored_files", "bnc_comments.rds"))

# Zip and save ------------------------------------------------------------

## save to temp folder
walk2(.x = bnc_threads$full_tags, .y =hoc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_full-tags.txt")))
walk2(.x = bnc_threads$tags_only, .y =hoc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_tags-only.txt")))

full_tags <-  list.files(path = out, pattern = "full-tags.txt", full.names = TRUE)
tags_only <- list.files(path = out, pattern = "tags-only.txt", full.names = TRUE)

zipr(zipfile = here("udpipe", "data","bnc_full_ud_tags.zip"), files = full_tags)
zipr(zipfile = here("udpipe", "data","bnc_ud_tags_only.zip"), files = tags_only)

# clean up ----------------------------------------------------------
rm(list =ls())

# HOC ----

# Paths -------------------------------------------------------------------
zipfile <- here("data", "hoc_txt.zip")
tmp <- tempfile()
junk <- tempdir("junk")
out <- tempdir("out")


# Unzip zip to tmp
unzip(zipfile, exdir = tmp)




# procezsing from zip -----------------------------------------------------
## get file paths and names
list_paths <- list.files(path = tmp, pattern = ".txt", full.names = TRUE)
files <- list_paths %>% basename %>% path_ext_remove()

# Unzip -------------------------------------------------------------------


# Process -----------------------------------------------------------------

 
## for threads use readtext()  
    threads <- tibble(file = files, path = list_paths) %>%
      mutate(
        text = map2(path, file, ~readtext(.x))) %>% 
      unnest(text) %>% 
      select(-c(path,doc_id))
    

## for comments use read_lines()  
   comments <-   tibble(file = files, path = list_paths) %>%
      mutate(
        text = map2(path, file, ~read_lines(.x))) %>% select(-path) %>% 
           unnest(text) %>% 
            group_by(file) %>% mutate(file=paste0(file,"_",row_number() %>% str_pad(3,"left","0")))
          
## load tagging model      
 mdl <- udpipe_load_model(file = here("BNC_clean", "data", "english-ewt-ud-2.5-191206.udpipe"))   
   

## getting full tags and tags only columns
hoc_threads <- threads %>%
      mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
      select(file,tagged) %>% 
      mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
      mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
      mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
      unnest(c(full_tags, tags_only))


hoc_comments <- comments %>%
  mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
  select(file,tagged) %>% 
  mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
  mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
  mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
  unnest(c(full_tags, tags_only))

## save as rdata

saveRDS(hoc_threads, file = here("udpipe","data", "hoc_threads.rds"))
saveRDS(hoc_comments, file = here("udpipe","data", "hoc_comments.rds"))

# Zip and save ------------------------------------------------------------

## save to temp folder
walk2(.x = hoc_threads$full_tags, .y =hoc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_full-tags.txt")))
walk2(.x = hoc_threads$tags_only, .y =hoc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_tags-only.txt")))

full_tags <-  list.files(path = out, pattern = "full-tags.txt", full.names = TRUE)
tags_only <- list.files(path = out, pattern = "tags-only.txt", full.names = TRUE)

zipr(zipfile = here("udpipe", "data","hoc_full_ud_tags.zip"), files = full_tags)
zipr(zipfile = here("udpipe", "data","hoc_ud_tags_only.zip"), files = tags_only)

# clean up ----------------------------------------------------------
rm(list =ls())

# JOC ----

# Paths -------------------------------------------------------------------
zipfile <- here("data", "joc_txt.zip")
tmp <- tempfile()
junk <- tempdir("junk")
out <- tempdir("out")


# Unzip zip to tmp
unzip(zipfile, exdir = tmp)




# procezsing from zip -----------------------------------------------------
## get file paths and names
list_paths <- list.files(path = tmp, pattern = ".txt", full.names = TRUE)
files <- list_paths %>% basename %>% path_ext_remove()

# Unzip -------------------------------------------------------------------


# Process -----------------------------------------------------------------

    
    ## for threads use readtext()  
    threads <- tibble(file = files, path = list_paths) %>%
      mutate(
        text = map2(path, file, ~readtext(.x))) %>% 
      unnest(text) %>% 
      select(-c(path,doc_id))
    
    
    ## for comments use read_lines()  
    comments <-   tibble(file = files, path = list_paths) %>%
      mutate(
        text = map2(path, file, ~read_lines(.x))) %>% select(-path) %>% 
      unnest(text) %>% 
      group_by(file) %>% mutate(file=paste0(file,"_",row_number() %>% str_pad(3,"left","0")))
    
    
    ## load tagging model      
    mdl <- udpipe_load_model(file = here("BNC_clean", "data", "english-ewt-ud-2.5-191206.udpipe"))   
    
    
    ## getting full tags and tags only columns
    joc_threads <- threads %>%
      mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
      select(file,tagged) %>% 
      mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
      mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
      mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
      unnest(c(full_tags, tags_only))
    
    
    joc_comments <- comments %>%
      mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
      select(file,tagged) %>% 
      mutate(full_tags = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
      mutate(full_tags = map(full_tags, ~ paste(.x, collapse = " "))) %>% 
      mutate(tags_only = map(tagged, ~ paste(.x$xpos, collapse = " "))) %>% 
      unnest(c(full_tags, tags_only))
    
    ## save as rdata
    
    saveRDS(joc_threads, file = here("udpipe","data", "joc_threads.rds"))
    saveRDS(joc_comments, file = here("udpipe","data", "joc_comments.rds"))
    
    # Zip and save ------------------------------------------------------------
    
    ## save to temp folder
    walk2(.x = joc_threads$full_tags, .y =joc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_full-tags.txt")))
    walk2(.x = joc_threads$tags_only, .y =joc_threads$file, function(x,y) write_lines(x, paste0(out,"/",y,"_tags-only.txt")))
    
    full_tags <-  list.files(path = out, pattern = "full-tags.txt", full.names = TRUE)
    tags_only <- list.files(path = out, pattern = "tags-only.txt", full.names = TRUE)
    
    zipr(zipfile = here("udpipe", "data","joc_full_ud_tags.zip"), files = full_tags)
    zipr(zipfile = here("udpipe", "data","joc_ud_tags_only.zip"), files = tags_only)
    
    

# Getting ngrams ----------------------------------------------------------


    
## hoc_threads and joc_threads can be found in udpipe/data
## bnc_threads is in ignored_files
    
 bnc_threads <-readRDS("~/Desktop/cmc-all/ignrored_files/bnc_threads.rds")
 hoc_threads <- read_rds(here("udpipe","data","hoc_threads.rds"))
 joc_threads <- read_rds(here("udpipe","data","joc_threads.rds"))

all_threads <- bind_rows(joc_threads, hoc_threads, bnc_threads) %>% 
                mutate(wc = str_count(.$tags_only, "\\b\\w+\\b"),
                       corpus = str_extract(.$file, "^..."), .after = file) 

## get table of wordcounts
all_threads %>% group_by(corpus) %>% 
  summarise(total_wc = sum(wc))

all_threads %>% ggplot(aes(wc)) +
  geom_histogram() +
  facet_wrap(~corpus, scales  = "free") +
  scale_x_continuous(trans = "log10") +
  theme_light()

all_threads %>% filter(wc >500) %>% 
  group_by(corpus) %>% 
  summarise(total_wc = sum(wc))

all_threads %>% filter(wc >500) %>% 
  count(corpus)

## create a reference column of file and word counts
wordcounts <- all_threads %>% select(file,wc)
## creating trigrams, bigrams and unigrams  ----



unigrams <- all_threads %>% 
  # remove commas, periods, colons and semi-colons as punctuation
  mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = unigram,input = tags_only, token = "words") %>%
  ungroup() %>% 
  # to avoid posgrams overlapping clause boundaries
  filter(!str_detect(unigram, "break")) %>% 
  # count the number per file
  count(file, unigram) %>% 
  # join with wordcounts
  inner_join(wordcounts) %>% 
  mutate(type = "unigram", .after = file) %>% 
  rename(posgram  = unigram)


bigrams <- all_threads %>% select(file,tags_only) %>% 
  # remove commas, periods, colons and semi-colons as punctuation
  mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = bigram,input = tags_only, token = "ngrams",n = 2) %>% ungroup() %>% 
  # to avoid posgrams overlapping clause boundaries
  filter(!str_detect(bigram, "break")) %>% 
  mutate(bigram = str_replace_all(.$bigram,' ','_')) %>% 
  # count the number per file
  count(file, bigram) %>% 
  # join with wordcounts
  inner_join(wordcounts) %>% 
  mutate(type = "bigram", .after = file) %>% 
  rename(posgram  = bigram)


trigrams <- all_threads %>% select(file,tags_only) %>% 
     # remove commas, periods, colons and semi-colons as punctuation
     mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = trigram,input = tags_only, token = "ngrams",n = 3) %>% ungroup() %>% 
     # to avoid posgrams overlapping clause boundaries
     filter(!str_detect(trigram, "break")) %>% 
      mutate(trigram = str_replace_all(.$trigram,' ','_')) %>% 
     # count the number per file
     count(file, trigram) %>% 
  # join with wordcounts
  inner_join(wordcounts) %>% 
mutate(type = "trigram", .after = file) %>% 
            rename(posgram  = trigram)


## join all togethet to make ngrmas
## add freq column
## nest for purrr
ngrams <-  bind_rows(unigrams,bigrams,trigrams) %>% 
            mutate(type = factor(type, levels = c("unigram","bigram","trigram")),
                  freq = n*1000/wc, .after = n) %>% 
  nest(data = c(file, posgram, n,freq, wc))


## PIVOT WIDER
full_pivot <- ngrams %>% 
  
  pivot_function <- 
  
  
  pivot_wider(names_from = posgram, values_from = freq, values_fill = 0) %>% 
  mutate(corpus = str_extract(.$file, "^..."), .after = file) %>% 
  inner_join(wordcounts) %>% 
  relocate(wc, .after = "corpus" )

## use pivot_function to pivot out wide
ngrams <-  ngrams %>% mutate(freq_table = map(data, ~pivot_function(.x)))



# ngrams %>% nest(-c(type))
# 
# 
# ## alternative to avoid remaking full_pivot
# wordcounts %>% arrange(file) %>% pull(wc) -> wc
# full_pivot %>% mutate(wc = wc, .after = corpus)
# 
# 
# ## convert to freq per 1000
# full_pivot_means <- full_pivot %>% mutate(across(4:length(full_pivot), ~.*1000/wc))
# 
# 
# # wc100_pivot <- ngrams_wc100 %>% 
#   pivot_wider(names_from =trigram, values_from = n, values_fill = 0) %>% 
#   mutate(corpus = str_extract(.$file, "^..."), .after = file)
# 
# wc500_pivot <- ngrams_wc500 %>% 
#   pivot_wider(names_from =trigram, values_from = n, values_fill = 0) %>% 
#   mutate(corpus = str_extract(.$file, "^..."), .after = file)

# trigram_totals <- full_pivot %>% group_by(corpus) %>% 
#   select(-wc) %>% 
#   summarise(across(where(is.integer), ~ mean((.x))))  %>% 
#   pivot_longer(cols = -corpus, names_to = "trigram", values_to = "mean_freq") 
# 
# trigram_totals_500 <- full_pivot %>% 
#   filter( wc >= 500) 
#   
#   
#   get_pivot(full_pivot)
#   
#   group_by(corpus) %>% 
#   select(-wc) %>% 
#   summarise(across(where(is.integer), ~ mean((.x))))  %>% 
#   pivot_longer(cols = -corpus, names_to = "trigram", values_to = "mean_freq") 
# 
# 
# nested_pivot <- nest(full_pivot, data = everything()) %>% 
#                 slice(rep(1, each=6)) %>% 
#                 mutate(threshold = c(seq(0,1000,200))) %>% 
#                 mutate(ngram_pivot = map2(.x = data, 
#                                              .y = threshold, 
#                                              .f = ~ .x %>% filter(wc >= .y) %>% 
#                                                get_pivot(.)))
# 
# nested_pivot <- nested_pivot %>% mutate(ngram_pivot = map2(.x = data, 
#                                               .y = threshold, 
#                                               .f = ~ .x %>% filter(wc >= .y) %>% 
#                                                 get_pivot(.))) %>% 
#                  mutate(top_10 = map(.x = ngram_pivot, .f = ~get_totals_10(.x))) %>%
#                  mutate(top_25 = map(.x = ngram_pivot, .f = ~get_totals_25(.x))) %>% 
#                  mutate(top_50 = map(.x = ngram_pivot, .f = ~get_totals_50(.x))) %>% 
#                 mutate(top_10_trigrams = map(.x = top_10, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
#                 mutate(top_25_trigrams = map(.x = top_25, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
#                 mutate(top_50_trigrams = map(.x = top_50, .f = ~.x %>% pull(trigram) %>% unique))
#   
# nested_pivot <- test
# 

# pivot functions ---------------------------------------------------------------


## this creates the frequency table
pivot_function <- function(x){
  x %>% 
    pivot_wider(id_cols = file,
                names_from = posgram, values_from = freq, values_fill = 0) %>% 
    mutate(corpus = str_extract(.$file, "^..."), .after = file) %>% 
    inner_join(wordcounts) %>% 
    relocate(wc, .after = "corpus" )
}


## this gets the mean posgrams for each corpus

get_pivot <- function(x){
  x %>% group_by(corpus) %>% 
    select(-wc) %>% 
    summarise(across(where(is.double), ~ mean((.x))))  %>% 
    pivot_longer(cols = -corpus, names_to = "posgram", values_to = "mean_freq")
}


## get_totals functions finding top x posgrams of each type

get_totals_100 <- function(x){
                     x %>%  group_by(corpus) %>% 
                      dplyr::slice_max(mean_freq,n = 100)
                 }
                 
get_totals_50 <- function(x){
                 x %>%  group_by(corpus) %>% 
                  dplyr::slice_max(mean_freq,n = 50)
}

get_totals_25 <- function(x){
 x %>%  group_by(corpus) %>% 
    dplyr::slice_max(mean_freq,n = 25)
}

get_totals_12 <- function(x){
  x %>%  group_by(corpus) %>% 
    dplyr::slice_max(mean_freq,n = 12)
}

## Use %notin% function
`%notin%` <- Negate(`%in%`)


# output for efa ----------------------------------------------------------

efa_input <- ngrams %>% 
  uncount(6) %>%  
  mutate(threshold = rep(seq(0,1000,200),3)) %>% 
  mutate(ngram_pivot = map2(.x = freq_table, 
                            .y = threshold, 
                            .f = ~ .x %>% filter(wc >= .y) %>% 
                              get_pivot(.)))   %>% #pluck("ngram_pivot",6) 
  mutate(top_12 = map(.x = ngram_pivot, .f = ~get_totals_12(.x))) %>%
  mutate(top_25 = map(.x = ngram_pivot, .f = ~get_totals_25(.x))) %>% 
  mutate(top_50 = map(.x = ngram_pivot, .f = ~get_totals_50(.x))) %>% 
  mutate(top_100 = map(.x = ngram_pivot, .f = ~get_totals_100(.x))) %>% 
  mutate(top_100_posgrams = map(.x = top_100, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
  mutate(top_50_posgrams = map(.x = top_50, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
  mutate(top_25_posgrams = map(.x = top_25, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
  mutate(top_12_posgrams = map(.x = top_12, .f = ~.x %>% pull(trigram) %>% unique))

## saving efa_frames

efa_100 <- efa_input %>% 
  mutate(freq_table = map2(.x = freq_table, 
                           .y = threshold, 
                           .f = ~ .x %>% filter(wc >= .y)), 
         .before = type) %>% 
  transmute(type = paste(type,threshold,sep = "_"),
            top = 100,
            data = map2(.x = freq_table,
                        .y = top_100_posgrams,
                        .f = ~.x %>% select(file,corpus,one_of(.y))),
            .before = type) 
efa_100

efa_50 <- efa_input %>% 
  mutate(freq_table = map2(.x = freq_table, 
                           .y = threshold, 
                           .f = ~ .x %>% filter(wc >= .y)), 
         .before = type) %>% 
  transmute(type = paste(type,threshold,sep = "_"),
            top = 50,
            data = map2(.x = freq_table,
                        .y = top_50_posgrams,
                        .f = ~.x %>% select(file,corpus,one_of(.y))),
            .before = type) 

efa_25 <- efa_input %>% 
  mutate(freq_table = map2(.x = freq_table, 
                           .y = threshold, 
                           .f = ~ .x %>% filter(wc >= .y)), 
         .before = type) %>% 
  transmute(type = paste(type,threshold,sep = "_"),
            top = 25,
            data = map2(.x = freq_table,
                        .y = top_25_posgrams,
                        .f = ~.x %>% select(file,corpus,one_of(.y))),
            .before = type) 

efa_12 <- efa_input %>% 
  mutate(freq_table = map2(.x = freq_table, 
                           .y = threshold, 
                           .f = ~ .x %>% filter(wc >= .y)), 
         .before = type) %>% 
  transmute(type = paste(type,threshold,sep = "_"),
            top = 12,
            data = map2(.x = freq_table,
                        .y = top_12_posgrams,
                        .f = ~.x %>% select(file,corpus,one_of(.y))),
            .before = type) 

efa_all <- bind_rows(efa_100,efa_50,efa_25,efa_12)


## save rds files to efa folder
saveRDS(efa_all, file = here::here("efa","data","efa_all.rds"))
saveRDS(efa_100, file = here::here("efa","data","efa_100.rds"))
saveRDS(efa_50, file = here::here("efa","data","efa_50.rds"))
saveRDS(efa_25, file = here::here("efa","data","efa_25.rds"))
saveRDS(efa_12, file = here::here("efa","data","efa_12.rds"))


# save data selecting all files > 500 wc and top 50 POStrigrams
# full_pivot %>% filter(file %notin% files_less_500 ) %>% 
#   select(file, corpus, one_of(trigrams_efa)) %>% 
#   write_csv(file = here::here("efa","data","efa_table.csv"))

