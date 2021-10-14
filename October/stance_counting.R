pacman::p_load( tidyverse, magrittr,gsubfn, tidytext,data.table)

rm(list = ls())
## this file loads in the wmatrix processed data
## filters for the cmc corpora
## cleans up the C7 tags
## splits them up by sentences
## the resulting cmc_dat is then saved as cmc_sentence_regex.rds

## it then loads the search regexes search_df, then adds cases to detect that deletion, search_df2

## It maps over the cmc_dat with all the regex searches to produce very large dataframes of all sentences with a count for each search

## These datframes are saved as JOC_stance_count and HOC_stance_count respectivelu
## load and clean the data ----
## load the wmatrix data
wmatrix_dat <- readRDS("~/Desktop/cmc-all/wmatrix/data/wmatrix_dat.rds")

## grab the cmc corpora only
cmc_dat <- wmatrix_dat %>% filter(corpus %in% c("hysoc","jusoc"))

## remove wmatrix data to save memory
rm(wmatrix_dat)


## create clean function
clean_C7 <-  function(df){
  df %>% mutate(C7 = str_remove(C7, "@"),
                C7 = str_remove(C7, "%")) 
}
## clean and modify cmc data
cmc_dat %<>%
  mutate(data = map(data, .f = ~ .x %>% rename(C7 = pos))) %>% 
  mutate(data = map(data, ~clean_C7(.x))) %>% 
  unnest(c(data)) %>% 
  select(-count) %>% 
  # standardise the corpus names
  mutate(corpus = case_when(
    corpus =="hysoc" ~ "HOC",
    corpus =="jusoc"  ~ "JOC")) %>% 
  # add corpus identifier to the front of each id
  mutate(id = case_when(
    corpus == "HOC" ~ paste0(corpus,"_",id),
    corpus == "JOC" ~ paste0(corpus,"_",id))) %>%
  transmute(corpus,id, tagged = paste(word,C7,lemma, sep = "_")) %>% 
  group_by(id) %>% 
  summarise(tagged_comments = paste(tagged, collapse = " ")) %>% 
 separate(id,c("corpus","id"),"_")


# regex details -----------------------------------------------------------

# functions for regex

# paste together tokens with the or pipe
paste_OR <- function(...){
  input <- list(...)
  items <- paste(sapply(input,paste,collapse = " "),collapse ="|")
  return(items)
  # return(paste0("(",items,")"))
}

# paste together tokens with a white space
paste_AND<- function(...){
  input <- list(...)
  items <- paste(sapply(input,paste,collapse = " "), collapse="\\s")
  return(paste0("(",items,")"))
}

# paste an optional marker
optional <- function(regex){
  return(paste0("(",regex,"\\s)?"))
}


# regexes on the fly 
anySpace <- "\\s"
anyWord <- "(([^_])+_(?!PUNC.)[^_ ]+_([^_ ])+)"
anyPron <- "(([^_ ])+_P([^U_ ])+_([^_ ])+)"
anyNoun <- "(([^_ ])+_N([^_ ])+_([^_ ])+)"
anyVerb <- "(([^_ ])+_V([^_ ])+_([^_ ])+)"
anyAdj <- "(([^_ ])+_JJ([^_ ])+_([^_ ])+)"
anyArt <- "(([^_ ])+_AT1?+_([^_ ])+)"
anyAdv <- "(([^_ ])+_R([^E_ ])+_([^_ ])+)"
anyDemP <- "(([^_ ])+_D([^_ ])+_([^_ ])+)"
anySubjPro <- "(([^_ ])+_PPY_([^_ ])+|([^_ ])+_PP.S._([^_ ])+)"
anyPossAdj <- "(([^_ ])+_APPGE([^_ ])+_([^_ ])+)"
anyThat <- "(([^_ ])+_RL_here)?(([^_ ])+_CST_that)"
anyPrep <- "(([^_ ])+_I[^_ ]_([^_ ])+)"
anyDet <- paste_OR(anyArt,anyDemP)
anySimpNounPhrase <- paste0(optional(anyDet),anyNoun,anySpace,anyPrep,anySpace,optional(anyDet),anyNoun)
anyCompNounPhrase <- paste0(optional(paste0(paste_OR(anyAdj,anyAdv,anyDet,anyPossAdj),anySpace)),optional(anyAdj),anyNoun,anySpace,anyPrep,optional(paste0(anySpace,paste_OR(anyDet,anyDemP))),anySpace,anyNoun)

search_df %>% clipr::write_clip()

# that deletion cases
## note when building long regexes, anySpace required between regex patterns
## except after optional() where space is already embedded

## case 1: match followed by demonstrative pronoun or subject pronoun 
case_1 <- paste_OR(anyDemP,anySubjPro)

# case 2: match followed by any pronoun or noun followed by any verb form
case_2 <- paste0(paste_OR(anyPron,anyNoun),anySpace,anyVerb)

# case 3: match followed by an adjective (JJ or PRED), an adverb (RB), a determiner (DT, QUAN, CD) 
# or a possessive pronoun (PRP$) and then a noun (N) and then a verb or auxiliary verb, 
# with the possibility of an intervening adjective (JJ or PRED) between the noun and its preceding word.

case_3 <- paste0(paste_OR(anyAdj,anyAdv,anyDet,anyPossAdj),anySpace,optional(anyAdj),anyNoun,anySpace,anyVerb)

## case 4: match followed by noun, then preposition, then optional determiner, then noun, then verb

case_4 <- paste0(anySimpNounPhrase,anySpace,anyVerb)


#read in the files
search_df <- read_delim(here::here("concordancing","data","stanceRegex4.txt"), delim ="\t")

search_that <- search_df %>% filter(str_detect(pattern,"(that)"))

search_others <- search_df %>% filter(!str_detect(pattern,"(that)"))


search_others %>% View

# getting JOC comment level stance count ----------------------------------------

## filter for JOC  corpus
comment_JOC <- cmc_dat %>% filter(corpus == "JOC")  %>% 
  transmute(corpus,id, comment =  tagged_comments)



## JOC  first
## extract that stances
## use pmap to iterate each search regex on the corpus data
tictoc::tic() ## 
JOC_stance_that_count <- pmap_df(search_that, ~comment_JOC %>% 
                                   mutate(
                                     pattern = ..4,
                                     regex = ..5,
                                     count = str_count(comment, ..5))) %>% 
  filter(count > 0) %>% 
  mutate(case_that = str_count(comment,paste0(regex,anyThat)),
         case_1 = str_count(comment,paste0(regex,case_1)),
         case_2 = str_count(comment,paste0(regex,case_2)),
         case_3 = str_count(comment,paste0(regex,case_3)),
         case_4 = str_count(comment,paste0(regex,case_4))) %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("case")), na.rm = T)) %>% 
  filter(sum >0) %>% 
  select(corpus,id,comment,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## extract other stances
## use pmap to iterate each search regex on the corpus data
tictoc::tic() 
JOC_stance_other_count <- pmap_df(search_others, ~comment_JOC %>% 
                                    mutate(
                                      pattern = ..4,
                                      regex = ..5,
                                      count = str_count(comment, ..5))) %>% 
  filter(count >0) %>% 
  select(corpus,id,comment,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## bind the two types together
JOC_stance_count <- do.call(bind_rows,list(JOC_stance_that_count,JOC_stance_other_count))


## join with the full sentences to see which ones are missing stances
JOC_stance_count <- comment_JOC %>% full_join(JOC_stance_count) %>% 
  mutate(count = ifelse(is.na(count),0,count)) %>% ungroup

## get untagged sentences and join with search_df to get Biber categories
JOC_stance_count %<>% mutate(tagged = comment,
                             comment = str_replace_all(comment,"([^_ ]+)([^ ]+)","\\1") %>% str_squish) %>% full_join(search_df) %>% ungroup() 


## add wordcount
JOC_stance_count %<>% mutate(wc = str_count(tagged,anyWord), .after = id)

## find then remove missing stances
JOC_comment_missing_stances <- JOC_stance_count %>% filter(is.na(id)) %>% 
  mutate(corpus = "JOC") %>% 
  select(corpus,pattern,Biber,category,subcategory)

JOC_comment_stance_count <- 
  JOC_stance_count %<>% filter(!is.na(id))



# getting count data for HOC comments ----------------------------------------------

comment_HOC <- cmc_dat %>% filter(corpus == "HOC") %>% 
  transmute(corpus,id, comment =  tagged_comments)



## extract stances with possible that clauses
## use pmap to iterate each search regex on the corpus data
tictoc::tic() ## 
HOC_stance_that_count <- pmap_df(search_that, ~comment_HOC %>% 
                                   mutate(
                                     pattern = ..4,
                                     regex = ..5,
                                     count = str_count(comment, ..5))) %>% 
  filter(count > 0) %>% 
  mutate(case_that = str_count(comment,paste0(regex,anyThat)),
         case_1 = str_count(comment,paste0(regex,case_1)),
         case_2 = str_count(comment,paste0(regex,case_2)),
         case_3 = str_count(comment,paste0(regex,case_3)),
         case_4 = str_count(comment,paste0(regex,case_4))) %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("case")), na.rm = T)) %>% 
  filter(sum >0) %>% 
  select(corpus,id,comment,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()




## extract other stances
## use pmap to iterate each search regex on the corpus data
tictoc::tic() 
HOC_stance_other_count <- pmap_df(search_others, ~comment_HOC %>% 
                                    mutate(
                                      pattern = ..4,
                                      regex = ..5,
                                      count = str_count(comment, ..5))) %>% 
  filter(count >0) %>% 
  select(corpus,id,comment,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()

## bind the two types together
HOC_stance_count <- do.call(bind_rows,list(HOC_stance_that_count,HOC_stance_other_count))


## join with the full sentences to see which ones are missing stances
HOC_stance_count <- sentence_HOC %>% full_join(HOC_stance_count)  %>% 
  mutate(count = ifelse(is.na(count),0,count)) %>% ungroup

## get untagged sentences and join with search_df to get Biber categories
HOC_stance_count %<>% mutate(tagged = comment,
                             comment = str_replace_all(comment,"([^_ ]+)([^ ]+)","\\1") %>% str_squish) %>% full_join(search_df) %>% ungroup() 

HOC_stance_count %<>% mutate(wc = str_count(tagged,anyWord), .after = id)




## find then remove missing stances
HOC_comments_missing_stances <- HOC_stance_count %>% filter(is.na(id)) %>% 
  mutate(corpus = "HOC") %>% 
  select(corpus,pattern,Biber,category,subcategory)

HOC_comment_stance_count <- 
  HOC_stance_count %>% filter(!is.na(id))




# create sentence level stance count --------------------------------------



## split the comments by sentences: cmc_sent
cmc_sent <- cmc_dat %>%   
  mutate(sentence =  tagged_comments %>% str_split(pattern = "-----_-----_PUNC")) %>% 
  unnest(sentence) %>% 
  na_if("") %>% 
  na.omit %>% 
  select(-tagged_comments)

## add sentence numbers

cmc_sent %<>% 
  group_by(id) %>% mutate(sentence_n = row_number(), .before = sentence) 




# getting the count data for JOC ------------------------------------------



## filter cmc_sent  for each corpus
sentence_JOC <- cmc_sent %>% filter(corpus == "JOC") 



case_1

## JOC only first

## extractstances with possible that clauses
tictoc::tic() ## 4 hours!
JOC_stance_that_count <- pmap_df(search_that, ~sentence_JOC %>% 
                 mutate(
                 pattern = ..4,
                 regex = ..5,
                 count = str_count(sentence, ..5))) %>% 
                 filter(count > 0) %>% 
  mutate(case_that = str_count(sentence,paste0(regex,anyThat)),
         case_1 = str_count(sentence,paste0(regex,case_1)),
         case_2 = str_count(sentence,paste0(regex,case_2)),
         case_3 = str_count(sentence,paste0(regex,case_3)),
         case_4 = str_count(sentence,paste0(regex,case_4))) %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("case")), na.rm = T)) %>% 
  filter(sum >0) %>% 
  select(corpus,id,sentence_n,sentence,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## extract other stances
tictoc::tic() 
JOC_stance_other_count <- pmap_df(search_others, ~sentence_JOC %>% 
                                   mutate(
                                     pattern = ..4,
                                     regex = ..5,
                                     count = str_count(sentence, ..5))) %>% 
  filter(count >0) %>% 
  select(corpus,id,sentence_n,sentence,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## bind the two types together
JOC_stance_count <- do.call(bind_rows,list(JOC_stance_that_count,JOC_stance_other_count))


## join with the full sentences to see which ones are missing stances
 JOC_stance_count <- sentence_JOC %>% full_join(JOC_stance_count) %>% 
  mutate(count = ifelse(is.na(count),0,count)) %>% ungroup

## get untagged sentences and join with search_df to get Biber categories
JOC_stance_count %<>% mutate(tagged = sentence,
                            sentence = str_replace_all(sentence,"([^_ ]+)([^ ]+)","\\1") %>% str_squish) %>% full_join(search_df) %>% ungroup() 


## add wordcount
JOC_stance_count %<>% mutate(wc = str_count(tagged,anyWord), .after = sentence_n)

## find then remove missing stances


JOC_sentence_stance_count <- JOC_stance_count %>% filter(!is.na(id))

JOC_sentence_stance_count %>% filter(subcategory == "epistemic verbs - likelihood")

# getting count data for HOC ----------------------------------------------



## use pmap to iterate each search regex on the corpus data
sentence_HOC <- cmc_sent %>% filter(corpus == "HOC")

## HOC only 

## extractstances with possible that clauses
tictoc::tic() ## 
HOC_stance_that_count <- pmap_df(search_that, ~sentence_HOC %>% 
                                   mutate(
                                     pattern = ..4,
                                     regex = ..5,
                                     count = str_count(sentence, ..5))) %>% 
  filter(count > 0) %>% 
  mutate(case_that = str_count(sentence,paste0(regex,anyThat)),
         case_1 = str_count(sentence,paste0(regex,case_1)),
         case_2 = str_count(sentence,paste0(regex,case_2)),
         case_3 = str_count(sentence,paste0(regex,case_3)),
         case_4 = str_count(sentence,paste0(regex,case_4))) %>%
  rowwise() %>%
  mutate(sum = sum(across(starts_with("case")), na.rm = T)) %>% 
  filter(sum >0) %>% 
  select(corpus,id,sentence_n,sentence,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## extract other stances
tictoc::tic() 
HOC_stance_other_count <- pmap_df(search_others, ~sentence_HOC %>% 
                                    mutate(
                                      pattern = ..4,
                                      regex = ..5,
                                      count = str_count(sentence, ..5))) %>% 
  filter(count >0) %>% 
  select(corpus,id,sentence_n,sentence,pattern,count) %>% 
  arrange(desc(corpus),id) 
tictoc::toc()


## bind the two types together
HOC_stance_count <- do.call(bind_rows,list(HOC_stance_that_count,HOC_stance_other_count))


## join with the full sentences to see which ones are missing stances
HOC_stance_count <- sentence_HOC %>% full_join(HOC_stance_count)  %>% 
  mutate(count = ifelse(is.na(count),0,count)) %>% ungroup

## get untagged sentences and join with search_df to get Biber categories
HOC_stance_count %<>% mutate(tagged = sentence,
                             sentence = str_replace_all(sentence,"([^_ ]+)([^ ]+)","\\1") %>% str_squish) %>% full_join(search_df) %>% ungroup() 

HOC_stance_count %<>% mutate(wc = str_count(tagged,anyWord), .after = sentence_n)

## find then remove missing stances
HOC_sentence_stance_count <- HOC_stance_count %>% filter(!is.na(id))

###### compare HOC and JOC----


# combine the dataframes
comment_stance_count <- do.call(bind_rows,list(HOC_comment_stance_count,JOC_comment_stance_count))

sentence_stance_count<- do.call(bind_rows,list(HOC_sentence_stance_count,JOC_sentence_stance_count))


# convert id to get thread details
thread_stance_count <- comment_stance_count %>% 
  mutate(text_id = id, .after = corpus) %>% 
  mutate(thread_id = str_extract(id,"(\\d{3}$)"), .after = id) %>% 
  mutate(id = str_remove(id,"\\d{3}$")) %>% rename(thread = id) 


## add counts per 10k
thread_stance_count %<>% group_by(thread) %>% mutate(thread_wc = sum(wc), .after = thread) %>% 
  ungroup() %>% 
  group_by(thread, pattern) %>% 
  mutate(pattern_per_10k = sum(count)*10000/thread_wc, .after = pattern) %>%
  ungroup() %>% 
  group_by(thread, subcategory) %>%
  mutate(subcat_per_10k = sum(count)*10000/thread_wc, .after = pattern_per_10k) %>%
  ungroup() %>% 
  group_by(thread, category) %>%
  mutate(category_per_10k = sum(count)*10000/thread_wc, .after = subcat_per_10k) %>%
  select(corpus,thread,pattern,subcategory,category,pattern_per_10k,subcat_per_10k,category_per_10k) %>% 
  ungroup()


### save to data folder

comment_stance_count %>% saveRDS(here::here("concordancing","data","comment_stance_count.rds"))

sentence_stance_count %>% saveRDS(here::here("concordancing","data","sentence_stance_count.rds"))

thread_stance_count %>% saveRDS(here::here("concordancing","data","thread_stance_count.rds"))


# plotting stance counts --------------------------------------------------


plot_stance_count <- thread_stance_count %>% # %>% filter(pattern_per_10k > 10) %>% 
mutate(corpus = factor(corpus, levels = c("JOC", "HOC")))

## create function to wrap any titles with new lines
swr <- function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr <-  Vectorize(swr)


plot_stance_count %>% filter(!is.na(pattern)) %>% 
  filter(category == "modal") %>% 
  mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
  ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
  coord_flip() +
  facet_wrap(~subcategory, scales = "free") +
  theme_minimal()  +
  labs(y = "Frequency per 10000 tokens", x = "Modal", title = "Boxplots of frequencies for the modal stance category")
  
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance adverb") %>% 
  #  filter(subcategory == "attitude") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance verb + (that) clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance verb + to-clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  var_width = 60


  

  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance adjective + (that) clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>%
    mutate(subcategory = swr(subcategory)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance adjective + to-clause") %>% 
    filter(subcategory != "evaluation adjectives + to") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>%  mutate(subcategory = swr(subcategory)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free", nrow = 3) +
    theme_minimal()  
 
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance noun + (that) clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  
  plot_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance noun + to-clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = corpus)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
 
  
  


# log odds ----------------------------------------------------------------

  library(tidylo)

  thread_stance_count %>% group_by(corpus,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup
  
  thread_stance_count %>% group_by(corpus,subcategory)  %>% 
    summarise(subcat_mean = mean(subcat_per_10k))
  
  thread_stance_count %>% group_by(corpus,category)  %>% 
    summarise(category_mean = mean(category_per_10k)) %>% 
  na.omit
  
  thread_stance_count %>% filter(is.na(category)) %>% count(corpus)
  

  
  
  pattern_lo <-    thread_stance_count %>% group_by(corpus,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup %>% 
    bind_log_odds(corpus, pattern, n) %>% 
    select(corpus,pattern,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      corpus=="JOC" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  # pattern_lo %>% filter(log_odds > 0.5) %>% pull(pattern) -> model_cols
  # plogis(0.983)
  # qlogis(0.8)
  # 
  subcat_lo <-    thread_stance_count %>% group_by(corpus,subcategory)  %>% 
    summarise(n = mean(subcat_per_10k)) %>% ungroup %>% 
    bind_log_odds(corpus, subcategory, n) %>% 
    select(corpus,subcategory,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      corpus=="JOC" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  subcat_lo %>% filter(log_odds > 0.4)
  
  category_lo <-    thread_stance_count %>% group_by(corpus,category)  %>% 
    summarise(n = mean(category_per_10k)) %>% ungroup %>% 
    bind_log_odds(corpus, category, n) %>% 
    select(corpus,category,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      corpus=="JOC" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  category_lo %>% filter(log_odds > 0.3)
  
 pattern_lo %>%
    group_by(corpus) %>%
    top_n(30, log_odds) %>%
    ungroup() %>%
    mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
    ggplot(aes(pattern, log_odds2, fill = corpus)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
 
 subcat_lo %>%
   group_by(corpus) %>% 
 #  filter(log_odds > 0) %>%
  top_n(10, log_odds) %>%
   ungroup() %>%
   mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
   ggplot(aes(subcategory, log_odds2, fill = corpus)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   theme_minimal()
 
 category_lo %>% filter(!is.na(category)) %>% 
   group_by(corpus) %>%
   filter(log_odds > 0) %>%
   ungroup() %>%
   mutate(category = fct_reorder(category, log_odds2)) %>%
   ggplot(aes(category, log_odds2, fill = corpus)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   theme_minimal()




# # save the outputs ------------------------------------------------------


 
 ## comment counts
 saveRDS(HOC_comment_stance_count,here::here("concordancing","data","HOC_comment_stance_count.rds"))
 saveRDS(JOC_comment_stance_count,here::here("concordancing","data","JOC_comment_stance_count.rds"))
 
 # sentence counts
 saveRDS(HOC_sentence_stance_count,here::here("concordancing","data","HOC_sentence_stance_count.rds"))
 saveRDS(JOC_comment_stance_count,here::here("concordancing","data","JOC_sentence_stance_count.rds"))

 ## missing stances
 saveRDS(HOC_comments_missing_stances,here::here("concordancing","data","HOC_missing_stances.rds"))
 
 saveRDS(JOC_comment_missing_stances,here::here("concordancing","data","JOC_missing_stances.rds"))
 
 JOC_comment_stance_count
 
 
#  
# saveRDS(cmc_dat,here::here("concordancing","data","cmc_sentence_regex.rds"))
# 
# saveRDS(JOC_stance_count,here::here("concordancing","data","JOC_stance_count.rds"))
# 
# saveRDS(HOC_stance_count,here::here("concordancing","data","HOC_stance_count.rds"))
# 
# 
# JOC <- 
# readRDS(here::here("concordancing","data","JOC_stance_count.rds"))
# 
#  