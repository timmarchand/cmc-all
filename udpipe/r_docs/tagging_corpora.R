
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

library(tidytext)   
library(tidylog)
    
## hoc_threads and joc_threads can be found in udpipe/data
## bnc_threads is in ignored_files
    
# bnc_threads <- read_rds(here("ignored_files","bnc_threads.rds"))
# hoc_threads <- read_rds(here("udpipe","data","hoc_threads.rds"))
# joc_threads <- read_rds(here("udpipe","data","joc_threads.rds"))

all_threads <- bind_rows(joc_threads, hoc_threads, bnc_threads) %>% 
                mutate(wc = str_count(.$tags_only, "\\b\\w+\\b"),
                       corpus = str_extract(.$file, "^..."), .after = file) 
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
    
threads_wc100 <- all_threads %>% filter(wc >100)
threads_wc500 <- all_threads %>% filter(wc >500)

ngrams <- all_threads %>% select(file,tags_only) %>% 
     # remove commas, periods, colons and semi-colons as punctuation
     mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = trigram,input = tags_only, token = "ngrams",n = 3) %>% ungroup() %>% 
     # to avoid trigrams overlapping clause boundaries
     filter(!str_detect(trigram, "break")) %>% 
      mutate(trigram = str_replace_all(.$trigram,' ','_')) %>% 
     # count the number per file
     count(file, trigram) 

ngrams_wc100 <- threads_wc100 %>% select(file,tags_only) %>% 
  # remove commas, periods, colons and semi-colons as punctuation
  mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = trigram,input = tags_only, token = "ngrams",n = 3) %>% ungroup() %>% 
  # to avoid trigrams overlapping clause boundaries
  filter(!str_detect(trigram, "break")) %>% 
  mutate(trigram = str_replace_all(.$trigram,' ','_')) %>% 
  # count the number per file
  count(file, trigram) 


ngrams_wc500 <- threads_wc500 %>% select(file,tags_only) %>% 
  # remove commas, periods, colons and semi-colons as punctuation
  mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>% 
  unnest_tokens(output = trigram,input = tags_only, token = "ngrams",n = 3) %>% ungroup() %>% 
  # to avoid trigrams overlapping clause boundaries
  filter(!str_detect(trigram, "break")) %>% 
  mutate(trigram = str_replace_all(.$trigram,' ','_')) %>% 
  # count the number per file
  count(file, trigram) 
     
## PIVOT WIDER
full_pivot <- ngrams %>% 
  pivot_wider(names_from =trigram, values_from = n, values_fill = 0) %>% 
  mutate(corpus = str_extract(.$file, "^..."), .after = file)

wc100_pivot <- ngrams_wc100 %>% 
  pivot_wider(names_from =trigram, values_from = n, values_fill = 0) %>% 
  mutate(corpus = str_extract(.$file, "^..."), .after = file)

wc500_pivot <- ngrams_wc500 %>% 
  pivot_wider(names_from =trigram, values_from = n, values_fill = 0) %>% 
  mutate(corpus = str_extract(.$file, "^..."), .after = file)

trigram_totals <- full_pivot %>% group_by(corpus) %>% 
  summarise(across(where(is.integer), ~ sum((.x))))  %>% 
  pivot_longer(cols = -corpus, names_to = "trigram", values_to = "total") 

trigram_wc100_totals <- wc100_pivot %>% group_by(corpus) %>% 
  summarise(across(where(is.integer), ~ sum((.x))))  %>% 
  pivot_longer(cols = -corpus, names_to = "trigram", values_to = "total") 



trigram_totals %>% group_by(corpus) %>% mutate(overall = sum(total)) %>% 
  ungroup() %>% 
  mutate(proportion = total/overall*1000) %>% 
  filter(proportion>2) %>% 
  tally()

# get the top 50 trigrams for each corpus
trigram_wc100_totals %>% group_by(corpus) %>% dplyr::slice_max(total,n = 50) %>% pull(trigram) -> top_50
top_50 %>% unique -> trigrams_efa

all_threads %>% count(corpus) -> all_count

threads_wc100 %>% count(corpus)

all_threads %>% filter(wc >500) %>% count(corpus) -> wc500_count

change <- tibble(corpus = all_count$corpus,
                 removed = all_count$n - wc500_count$n)

wc500_count %>% summarise(sum = sum(n))


## selecting trigrams in full pivot table
full_pivot %>% select(file, corpus, one_of(trigrams_efa))

## check if any are dropped by filtering for low proportions
trigram_totals %>% filter(trigram %in% trigrams_efa) %>% filter(total > 0) # tidylog result: none removed

## check by proportion one per 1000
trigram_totals %>% group_by(corpus) %>% mutate(overall = sum(total)) %>% 
  ungroup() %>% 
  mutate(proportion = total/overall*1000) %>% 
  filter(trigram %in% trigrams_efa) %>%
  filter(proportion>1) # tidylog result: 163 removed (22%)

## check by proportion one per 10000
trigram_totals %>% group_by(corpus) %>% mutate(overall = sum(total)) %>% 
  ungroup() %>% 
  mutate(proportion = total/overall*10000) %>% 
  filter(trigram %in% trigrams_efa) %>%
  filter(proportion>1) # tidylog result: 8 removed (1%)

files_less_500 <- all_threads %>% filter(wc <= 500) %>% pull(file)


## Use %notin% function
`%notin%` <- Negate(`%in%`)

full_pivot %>% filter(file %notin% files_less_500 ) %>% 
  select(file, corpus, one_of(trigrams_efa))

