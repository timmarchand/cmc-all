## Libraries ---------------------------------------------------------------
suppressMessages({
  library(here)
  library(tidyverse)
  library(XML)
  # library(methods)
  library(rvest)
  library(fs)
  library(rlang)
  library(magrittr) # for the %<>% operator
  library(zip)
  library(dplyr)
  library(udpipe)
})



# functions ---------------------------------------------------------------

## function to read the base xml files, while switching punctuation tags "<c>" to character tags "<w>"

parse_file <- function(file) {
  
  #file <- eval_tidy(file)
  file <- gsub("<c", "<w", file)
  file <- gsub("</c", "</w", file)
  xml <- xmlParse(file)
  saveXML(xml,paste0(junk,"/parse.xml"))
  xml <- read_xml(paste0(junk,"/parse.xml"))
  content <- xml %>% xml_nodes("div[level='1']")
  ### remove new xml file so that it is ignored in the following loop
  unlink(paste0(junk,"/parse.xml"))
  ## this divides the dem XML structure
  if (length(content) == 0) {
    content <- xml %>%
      xml_nodes("div")
  }
  return(content)
}


## parses bnc xml by grabbimg "s" nodes, and combining new sentences with linebreaks
parse_text <- function(content){
  text<- map(content, ~.x %>% xml_nodes("s") %>% html_text() %>% paste(collapse = "\n"))
  return(text)
}

## same function as parse_text, except keeps senteneces vectorised



## function to combine word_pos_lemma (t3_t2_t1) into one text string from bnc xml output
arrange_tags <- function(x){
  t <-  x %>% xml_nodes("w")
  t3<-t %>% html_text('table')
  t2<-t %>% xml_attr("c5")
  t1<- t %>% xml_attr("hw")
  ## replace NA with the same punc
  e<-which(is.na((t1)))
  t1[e]<-str_trim(t3)[e]
  ## combine the 3 attributes
  tags <-(paste0(str_trim(t3),"_",t2,"_",t1))
  tags <-paste0(tags,collapse=" ")
  tags<-gsub("_NA","",tags)
  return(tags)
}

# function to map arrange_tags on bnc file input
parse_tags <- function(content){
  tags<- map(content, ~.x %>% arrange_tags)
  return(tags)
}
##
# parse_udpipe <- function(x){
#   model <-  udpipe(x,  object = mdl)
#   return(model)
# }

## paths ---------------------------------------------------------------

zipfile <- here("BNC_Clean","data", "BNC.zip")
tmp <- tempfile()
junk <- tempdir("junk")
out <- tempdir("out")


# Unzip bnc.zip to tmp
unzip(zipfile, exdir = tmp)




# procezsing from zip -----------------------------------------------------
## get file paths and names
list_paths <- list.files(path = tmp, pattern = ".xml", full.names = TRUE)
files <- list_paths %>% basename %>% path_ext_remove()

## create tibble


dat <- tibble(file = files, path = list_paths) %>%
  # optional filtering
  # filter( file == "news_K5K" | file == "news_K5E") %>%
  mutate(
    # map two inputs, path (x) and file name (y)
    text =  map2(path, file, function(x, y)
      x %>% read_file() %>%
        parse_file() %>%  parse_text() %>%
        setNames(paste(y, seq_along(.), sep = '_'))),
    tags =  map2(path, file, function(x, y)
      x %>% read_file() %>%
        parse_file() %>%  parse_tags() %>%
        setNames(paste(y, seq_along(.), sep = '_')))
  )

## unnest to create df of text and tags
df <- dat %>% select(file, text, tags) %>%
  unnest(c(text,tags)) %>%
  unnest(c(text,tags)) %>%
  group_by(file)   %>% 
  mutate(id = paste0(file,"_",row_number() %>% str_pad(3,"left","0"))) %>%
  relocate(id,1) %>% 
  ungroup()

## save as rdata

saveRDS(dat, file = here("BNC_clean","data", "bnc_dat.rds"))
saveRDS(bnc_df, file = here::here("BNC_clean","data", "bnc_df.rds"))


## apply udpipe model ---------

# load udpipe model and rds data

mdl <- udpipe_load_model(file = here("BNC_clean", "data", "english-ewt-ud-2.5-191206.udpipe"))
df <- readRDS(file = here("BNC_clean","data", "bnc_df.rds"))

# define column names to be integers
ints <- c("paragraph_id", "sentence_id", "start", "end", "term_id", "token_id", "head_token_id")

ud_mdl <- df[1,] %>%
  mutate(model = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
  select(id,model) %>% unnest(model) %>%  
  mutate(across(all_of(ints), as.integer)) 
 
  
  ud_tag <- df %>% head(2) %>% 
    mutate(tagged = map(text, ~udpipe(.x, object = mdl, parrallel.cores = 1L)))  %>%
    mutate(tagged = map(tagged, ~paste(.x$token, .x$lemma, .x$xpos, sep = "_"))) %>% 
    mutate(tagged = map(tagged, ~ paste(.x, collapse = " "))) %>% 
    unnest(tagged) %>% 
    group_by(file) %>% mutate(file=paste0(file,"_",row_number() %>% str_pad(3,"left","0"))) %>% 
    select(file,tagged)
# saving into zip files ---------------------------------------------------

##  create temp out folder if necessary
out <- tempdir("out")

## walk text files to temp out folder
walk2(.x = df$text, .y =df$id, function(x,y) write_lines(x, paste0(out,"/",y,".txt") ))

walk2(.x = df$tags, .y =df$id, function(x,y) write_lines(x, paste0(out,"/",y,"_tags.txt") ))

walk2(.x = ud_tag$tagged, .y =ud_tag$id, function(x,y) write_lines(x, paste0(out,"/",y,"_ud-tags.txt") ))





#  create out paths
out_txts <- list.files(path = out, pattern = ".txt", full.names = TRUE)
out_tags <- list.files(path = out, pattern = "_tags.txt", full.names = TRUE)
ud_tags <-  list.files(path = out, pattern = "ud-tags.txt", full.names = TRUE)


# not in function
`%notin%` <- Negate(`%in%`)

# remove tagged text paths 
out_txts <- out_txts[out_txts %notin% out_tags]

## create zip files
zipr(zipfile = here("BNC_clean", "data","bnc_txt.zip"),files = out_txts)
zipr(zipfile = here("BNC_clean", "data","bnc_tag_c5.zip"),files = out_tags)
zipr(zipfile = here("BNC_clean", "data","bnc_tag_ud.zip"), files = ud_tags)



# clean up ----------------------------------------------------------
rm(list =ls())