# Libraries ----------------------------------------------------------

library(tidytext)   
library(tidylog)


# Data --------------------------------------------------------------------



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



# Functions ---------------------------------------------------------------
get_pivot <- function(x){
  x %>% group_by(corpus) %>% 
    select(-wc) %>% 
    summarise(across(where(is.integer), ~ sum((.x))))  %>% 
    pivot_longer(cols = -corpus, names_to = "trigram", values_to = "total")
}

get_totals_10 <- function(x){
  x %>%  group_by(corpus) %>% 
    dplyr::slice_max(total,n = 10)
}

get_totals_50 <- function(x){
  x %>%  group_by(corpus) %>% 
    dplyr::slice_max(total,n = 50)
}

get_totals_25 <- function(x){
  x %>%  group_by(corpus) %>% 
    dplyr::slice_max(total,n = 25)
}

nest_filter <- function(x,y){
  x %>% filter(wc >= y)
}

`%notin%` <- Negate(`%in%`)


select_trigrams <- function(x,y){
  x %>% select(file,corpus,one_of(!!y))
}




# Data wrangling ----------------------------------------------------------


## create a reference column of file and word counts
wordcounts <- all_threads %>% select(file,wc)

full_pivot <- all_threads %>% select(file,tags_only) %>%
  # remove commas, periods, colons and semi-colons as punctuation
  mutate(tags_only = map_chr(tags_only, ~str_replace_all(.x, "[,\\.:;]", "BREAK"))) %>%
  unnest_tokens(output = trigram,input = tags_only, token = "ngrams",n = 3) %>% ungroup() %>%
  # to avoid trigrams overlapping clause boundaries
  filter(!str_detect(trigram, "break")) %>%
  mutate(trigram = str_replace_all(.$trigram,' ','_')) %>%
  # count the number per file
  count(file, trigram) %>%
  # join with wordcounts
  inner_join(wordcounts)


## alternative to avoid remaking full_pivot
wordcounts %>% arrange(file) %>% pull(wc) -> wc
full_pivot %>% mutate(wc = wc, .after = corpus)


## convert to freq per 1000
full_pivot_means <- full_pivot %>% mutate(across(4:length(full_pivot), ~.*1000/wc))

## nested_pivot has 6 versions of the data with word count thresholds included
nested_pivot <- nest(full_pivot, data = everything()) %>% 
  bind_cols(nest(full_pivot, data_means = everything())) %>% 
  # duplicate rows 6 times
  slice(rep(1, each=6)) %>% 
  # add threshold column
  mutate(threshold = c(seq(0,1000,200))) %>% 
  # modify data to exclude those below the threshold
  mutate(data = map2(.x = data, 
                     .y = threshold, 
                     .f = ~ nest_filter(.x,.y))) %>% 
  mutate(data_means = map2(.x = data_means, 
                           .y = threshold, 
                           .f = ~ nest_filter(.x,.y))) %>% 
# apply ngram function on data
mutate(ngram_pivot = map(.x = data, .f = ~get_pivot(.x)))


## extended_pivot creates pos trigram frequency tables for the top 10, 25 and 50 trigrms in each corpus
extended_pivot <- nested_pivot %>% 
  mutate(top_10 = map(.x = ngram_pivot, .f = ~get_totals_10(.x))) %>%
  mutate(top_25 = map(.x = ngram_pivot, .f = ~get_totals_25(.x))) %>% 
  mutate(top_50 = map(.x = ngram_pivot, .f = ~get_totals_50(.x))) %>% 
  mutate(top_10_trigrams = map(.x = top_10, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
  mutate(top_25_trigrams = map(.x = top_25, .f = ~.x %>% pull(trigram) %>% unique)) %>% 
  mutate(top_50_trigrams = map(.x = top_50, .f = ~.x %>% pull(trigram) %>% unique))



## efa_input takes selects only the columns of the top_x trigrams 

efa_input <- extended_pivot %>% 
  mutate(efa_input_10 = map2(.x = data_means, .y = top_10_trigrams, 
                            .f = ~select(.x, file, corpus, wc, one_of(.y)))) %>% 
  mutate(efa_input_25 = map2(.x = data_means, .y = top_25_trigrams, 
                            .f = ~select(.x, file, corpus, wc, one_of(.y)))) %>% 
  mutate(efa_input_50 = map2(.x = data_means, .y = top_50_trigrams,
                             .f = ~select(.x, file, corpus, wc, one_of(.y))))

## select as input columns only the most frequent trigrams 

efa_input <- 
  efa_input %>% 
  mutate(
  data_efa_10 = map2(.x = data_means, .y = top_10_trigrams,
                      .f = ~select_trigrams(.x,.y))) %>% 
  mutate(
  data_efa_25 = map2(.x = data_means, .y = top_25_trigrams,
                      .f = ~select_trigrams(.x,.y))) %>% 
  mutate(
  data_efa_50 = map2(.x = data_means, .y = top_50_trigrams,
                       .f = ~select_trigrams(.x,.y))) %>% 
  select(threshold, starts_with("data_efa"))


# save data selecting
efa_input %>% 
  saveRDS(file = here::here("efa","data","efa_input_table.rds"))
