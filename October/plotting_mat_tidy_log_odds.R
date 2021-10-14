library(tidyverse, tidylo)
## get data from mat-tagger data folder

dimtag_stats <- readRDS(here::here("mat-tagger","data","dimtag_stats.rds"))

dimtag_stats %>% bind_rows() %>% 
  mutate(corpus = str_extract(text_id,"^...")) %>% filter(corpus %in% c("JOC","HOC")) %>% 
  select(corpus, everything(),-text_id,-awl,-ttr,-wordcount) %>% 
  pivot_longer(cols = -corpus, names_to = "tag", values_to = "freq")  %>% 
  group_by(corpus,tag) %>% 
  summarise(mean_freq = mean(freq, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_log_odds(set = corpus, feature = tag, n = mean_freq, unweighted = TRUE, uninformative = TRUE) %>%
  #rename(log_odds = log_odds_weighted) %>% 
  left_join(tagful) %>% 
  filter(tag %in% tag4) %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 6)  %>% 
  ungroup() %>%
  mutate(tagfull = fct_reorder(tagfull, log_odds2)) %>%
  ggplot(aes(tagfull, log_odds2, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()


# Plot dimension 1 weighted log odds
# note there are 26 tags to compare
dimtag_stats[[1]] %>% 
  mutate(corpus = str_extract(text_id,"^...")) %>% filter(corpus %in% c("JOC","HOC")) %>% 
  # remove the word length and ttr tags as they are not normalised
  select(corpus, everything(),-text_id,-awl,-ttr,-wordcount) %>% 
  pivot_longer(cols = -corpus, names_to = "tag", values_to = "freq") %>% 
  group_by(corpus,tag) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ungroup() %>% 
  bind_log_odds(set = corpus, feature = tag, n = mean_freq) %>% 
  rename(log_odds = log_odds_weighted) %>% 
  left_join(tagful) %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 26)  %>% 
  ungroup() %>%
  mutate(tagfull = fct_reorder(tagfull, log_odds2)) %>%
  ggplot(aes(tagfull, log_odds2, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Weighted log odds ratio", x =" Dimension 1 features", title = "Weighted log odds of Dimension 1 features", subtitle = "(positive scores show increased frequencies in the reference corpus)")

# Plot dimension 2 weighted log odds
## note there are 6 tags to compare
dimtag_stats[[2]] %>% 
  mutate(corpus = str_extract(text_id,"^...")) %>% filter(corpus %in% c("JOC","HOC")) %>% 
  select(corpus, everything(),-text_id,-wordcount) %>% 
  pivot_longer(cols = -corpus, names_to = "tag", values_to = "freq") %>% 
  group_by(corpus,tag) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ungroup() %>% 
  bind_log_odds(set = corpus, feature = tag, n = mean_freq) %>% 
  rename(log_odds = log_odds_weighted) %>% 
  left_join(tagful) %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 6)  %>% 
  ungroup() %>%
  mutate(tagfull = fct_reorder(tagfull, log_odds2)) %>%
  ggplot(aes(tagfull, log_odds2, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

# Plot dimension 3 weighted log odds
## note there are 8 tags to compare
dimtag_stats[[3]] %>% 
  mutate(corpus = str_extract(text_id,"^...")) %>% filter(corpus %in% c("JOC","HOC")) %>% 
  select(corpus, everything(),-text_id,-wordcount) %>% 
  pivot_longer(cols = -corpus, names_to = "tag", values_to = "freq") %>% 
  group_by(corpus,tag) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ungroup() %>% 
  bind_log_odds(set = corpus, feature = tag, n = mean_freq) %>% 
  rename(log_odds = log_odds_weighted) %>% 
  left_join(tagful) %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 8)  %>% 
  ungroup() %>%
  mutate(tagfull = fct_reorder(tagfull, log_odds2)) %>%
  ggplot(aes(tagfull, log_odds2, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()


# Plot dimension 4 weighted log odds
## note there are 6 tags to compare
dimtag_stats[[4]] %>% 
  mutate(corpus = str_extract(text_id,"^...")) %>% filter(corpus %in% c("JOC","HOC")) %>% 
  select(corpus, everything(),-text_id,-wordcount) %>% 
  pivot_longer(cols = -corpus, names_to = "tag", values_to = "freq") %>% 
  group_by(corpus,tag) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ungroup() %>% 
  bind_log_odds(set = corpus, feature = tag, n = mean_freq) %>% 
  rename(log_odds = log_odds_weighted) %>% 
  left_join(tagful) %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 6)  %>% 
  ungroup() %>%
  mutate(tagfull = fct_reorder(tagfull, log_odds2)) %>%
  ggplot(aes(tagfull, log_odds2, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()
  
