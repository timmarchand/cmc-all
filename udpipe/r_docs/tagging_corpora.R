
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

saveRDS(bnc_threads, file = here("udpipe","data", "bnc_threads.rds"))
saveRDS(bnc_comments, file = here("udpipe","data", "bnc_comments.rds"))

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

all_threads <- bind_rows(joc_threads, hoc_threads, bnc_threads)
    
ngrams <- all_threads %>% select(file,tags_only) %>% 
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

trigram_totals <- full_pivot %>% group_by(corpus) %>% 
  summarise(across(where(is.integer), ~ sum((.x))))  %>% 
  pivot_longer(cols = -corpus, names_to = "trigram", values_to = "total") 


trigram_totals %>% filter(total >2000) %>% filter(corpus == "dem")
