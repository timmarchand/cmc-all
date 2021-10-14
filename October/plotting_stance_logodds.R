# plotting log odds for stance --------------------------------------------
## libraries

pacman::p_load(tidyverse,tidylo)

## get the data

comment_stance_count <- readRDS(here::here("concordancing","data","comment_stance_count"))


## functions

## two functions: key_tbl just requires df input with the shape specified
## for get_key_tbl, necessary to specify column names

## function to replicate Rayson Excel sheet
# Returns the Loglikelihood (LL)
#             Percentage Difference(%DIFF)
#             Bayes Factor BIC (BIC)
#             Effect size for LL (ELL)
#             Relative risk (rel_risk)
#             Log ratio (log_ratio)
#             Odds ratio (odds_ratio)
#             Log Odds (log_odds)

## required input: dataframe with 
# item in column1,
# item count for corpus 1 in column 2,
# item count for corpus 2 in column 3,
# total corpus 1 token count in column 4,
# total corpus 2 token count in column 5

key_tbl <- function(df){
  tbl <- tibble(
    item = df %>% pull(1),
    c1 = df %>% pull(2),
    c2 = df %>% pull(3),
    c1_sum = df %>% pull(4),
    c2_sum = df %>% pull(5),
    # expected values = item count in corpus * (freq in corpus 1 & 2 / item count in both corpora)
    extd_c1 = c1_sum*((c1+c2)/(c1_sum+c2_sum)),
    extd_c2 = c2_sum*((c1+c2)/(c1_sum+c2_sum)),
    LL = 2*(c1*ifelse(c1 == 0 |extd_c1 == 0, 0, log(c1/extd_c1)) + c2*ifelse(c2 ==0 |extd_c2 == 0, 0, log(c2/extd_c2))),
    # normalised freq, cf probability, 
    # freq in corpus / total item count in corpus
    c1_norm = c1/c1_sum,
    c2_norm = c2/c2_sum,
    `%DIFF` = 100*(c1_norm - c2_norm)/ifelse(c2_norm == 0 , 1E-18,c2_norm),
    BIC = LL - log(c1_sum+c2_sum),
    ELL = LL/((c1_sum+c2_sum)*log(pmin(extd_c1,extd_c2))),
    rel_risk = c1_norm/c2_norm,
    # log_ratio = log2(normalised freq in corpus1/ normalised freq in corpus)
    # in case either are 0, divide 0.5 by total number of items in the corpus
    log_ratio = log2(ifelse(c1_norm == 0,0.5/c1_sum,c1_norm)/ifelse(c2_norm == 0,0.5/c2_sum,c2_norm)),
    # odds_ratio = (freq in corpus1/ freq not in corpus1) / 
    # (freq in corpus2 / freq not in corpus2)
    odds_ratio = (c1/(c1_sum-c1))/(c2/(c2_sum-c2)),
    log_odds = log(odds_ratio)
    
  )
  tbl %>% select(item,LL,`%DIFF`,BIC,ELL,rel_risk,log_ratio,odds_ratio,log_odds)
}



zxc <- tibble(a = c(1,2,3,4,5,6),
              b = c(0,2,4,6,8,10),
              c = c(6,4,2,1,0,0),
              d = c("zero"))

zxc %>% mutate(result = ifelse( b == 0 | c == 0 , d, "not_zero"))


## get_key_tbl - specify column names for:
# item
# freq_c1 = frequency of item in corpus 1
# freq_c2 = frequency of item in corpus 2
# sum_c1  = total count for corpus 1
# sum_c2  = total count for corpus 2



get_key_tbl <- function(tbl,item,freq_c1,freq_c2,sum_c1,sum_c2){
  item <- enquo(item)
  c1 <- enquo(freq_c1)
  c2 <- enquo(freq_c2)
  sum_c1 <- enquo(sum_c1)
  sum_c2 <- enquo(sum_c2)
  grouping <- group_vars(tbl)
  tbl <- ungroup(tbl)
  # expected values = item count in corpus * (freq in corpus 1 & 2 / item count in both corpora)
  tbl <- tbl %>% mutate(
    extd_c1 = !!sum_c1*((!!c1+!!c2)/(!!sum_c1+!!sum_c2)),
    extd_c2 = !!sum_c2*((!!c1+!!c2)/(!!sum_c1+!!sum_c2)),
    LL = 2*(c1*ifelse(c1 == 0 |extd_c1 == 0, 0, log(c1/extd_c1)) + c2*ifelse(c2 ==0 |extd_c2 == 0, 0, log(c2/extd_c2))),
    # normalised freq, cf probability, 
    # freq in corpus / total item count in corpus
    c1_norm = !!c1/!!sum_c1,
    c2_norm = !!c2/!!sum_c2,
    `%DIFF` = 100*(c1_norm - c2_norm)/ifelse(c2_norm == 0 , 1E-18,c2_norm),
    BIC = LL - log(!!sum_c1+!!sum_c2),
    ELL = LL/((!!sum_c1+!!sum_c2)*log(pmin(extd_c1,extd_c2))),
    rel_risk = c1_norm/c2_norm,
    # log_ratio = log2(normalised freq in corpus1/ normalised freq in corpus)
    # in case either are 0, divide 0.5 by total number of items in the corpus
    log_ratio = log2(ifelse(c1_norm == 0,0.5/!!sum_c1,c1_norm)/ifelse(c2_norm == 0,0.5/!!sum_c2,c2_norm)),
    # odds_ratio = (freq in corpus1/ freq not in corpus1) / 
    # (freq in corpus2 / freq not in corpus2)
    odds_ratio = (!!c1/(!!sum_c1-!!c1))/(!!c2/(!!sum_c2-!!c2)),
    log_odds = log(odds_ratio)
  )
  tbl %>% select(-extd_c1,-extd_c2) 
}




## Traditional log odds

## create tables for input

stance_pattern_tbl <- comment_stance_count %>% 
  count(pattern, by = corpus) %>% 
  pivot_wider(names_from = by, values_from = n, values_fill = 0) %>% 
  mutate(HOC_wc = 1538720, JOC_wc = 546699)


stance_pattern_tbl %>% clipr::write_clip()

stance_subcategory_tbl <- comment_stance_count %>%
  count(subcategory, by = corpus) %>% 
  pivot_wider(names_from = by, values_from = n, values_fill = 0) %>% 
  mutate(HOC_wc = 1538720, JOC_wc = 546699) 

stance_category_tbl <- 
  comment_stance_count %>% 
  count(category, by = corpus) %>% 
  pivot_wider(names_from = by, values_from = n, values_fill = 0) %>% 
  mutate(HOC_wc = 1538720, JOC_wc = 546699) 

## apply function and visualise
key_tbl(stance_category_tbl) %>% 
  mutate(abs= abs(log_odds)) %>% 
  mutate(pos = log_odds <= 0)%>% na.omit %>% 
  top_n(20, abs) %>%
  mutate(item = fct_reorder(item, log_odds)) %>%
  ggplot(aes(item, log_odds, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

key_tbl(stance_subcategory_tbl) %>% 
  mutate(abs= abs(log_odds)) %>% 
  mutate(pos = log_odds <= 0)%>% na.omit %>% 
  top_n(20, abs) %>%
  mutate(item = fct_reorder(item, log_odds)) %>%
  ggplot(aes(item, log_odds, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

key_tbl(stance_pattern_tbl) %>%
  mutate(abs = abs(log_odds)) %>% 
  mutate(pos = log_odds <= 0)%>% na.omit %>% 
  slice_max(abs_log_odds, n = 50) %>%
  mutate(item = fct_reorder(item, log_odds)) %>%
  ggplot(aes(item, log_odds, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()


##  %DIFF

## create tables for input


## apply function and visualise
key_tbl(stance_category_tbl) %>% 
   mutate(abs = abs(`%DIFF`)) %>% 
  mutate(pos = `%DIFF` <= 0)%>% na.omit %>% 
  top_n(20, abs) %>%
  mutate(item = fct_reorder(item, `%DIFF`)) %>%
  ggplot(aes(item, `%DIFF`, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

key_tbl(stance_subcategory_tbl) %>% 
  mutate(abs = abs(`%DIFF`)) %>% 
  mutate(pos = `%DIFF` <= 0)%>% na.omit %>% 
  top_n(20, abs) %>%
  mutate(item = fct_reorder(item,`%DIFF`)) %>%
  ggplot(aes(item, `%DIFF`, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

key_tbl(stance_pattern_tbl) %>% 
  mutate(abs = abs(`%DIFF`)) %>% 
  mutate(pos = `%DIFF` <= 0)%>% na.omit %>% 
  top_n(30, abs) %>%
  mutate(item = fct_reorder(item,`%DIFF`)) %>%
  ggplot(aes(item, `%DIFF`, fill = pos)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()

## create stance table for tidylo input
stance_tidylo_tbl  <- 
  comment_stance_count %>% 
  ## add counts for all other tokens in the corpus subtracting stance totals from corpus totals
  count(corpus, name = "count") %>% 
  mutate(count = case_when(corpus == "HOC" ~ 1538720 - count,
                           TRUE ~ 546699 - count )) %>% 
  ## add "other" to specified columns
  mutate(pattern = "other",subcategory = "other", category = "other") %>% 
  ## bind with stance count
  bind_rows(comment_stance_count) 


## plot for pattern
stance_tidylo_tbl %>% 
  count(pattern, by = corpus) %>% 
  # remove NA from pattern
  filter(!is.na(pattern)) %>%
  bind_log_odds(set = by, feature = pattern, n = n) %>% 
  select(corpus = by,pattern,n, log_odds = log_odds_weighted) %>% 
  filter(pattern != "other") %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 20)  %>% 
  ungroup() %>%
  mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
  ggplot(aes(pattern, log_odds2, fill = corpus)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Weighted log odds ratio", x = "Stance pattern", title = "Top 20 contrasting stance patterns", subtitle = "Calculated by weighted log odds ratios of frequency counts") +
  theme_minimal()


## plot for subcategory
stance_tidylo_tbl %>% 
  count(subcategory, by = corpus) %>% 
  # remove NA from pattern
  # filter(!is.na(pattern)) %>%
  bind_log_odds(set = by, feature = subcategory, n = n) %>% 
  select(corpus = by,subcategory,n, log_odds = log_odds_weighted) %>% 
  filter(subcategory != "other") %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 20)  %>% 
  ungroup() %>%
  mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
  ggplot(aes(subcategory, log_odds2, fill = corpus)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Weighted log odds ratio", x = "Stance pattern", title = "Top 20 contrasting stance subcategories", subtitle = "Calculated by weighted log odds ratios of frequency counts") +
  theme_minimal()

## plot for category
stance_tidylo_tbl %>% 
  count(category, by = corpus) %>% 
  # remove NA from pattern
  # filter(!is.na(pattern)) %>%
  bind_log_odds(set = by, feature = category, n = n) %>% 
  select(corpus = by,category,n, log_odds = log_odds_weighted) %>% 
  filter(category != "other") %>% 
  mutate(log_odds2 = case_when(
    corpus =="JOC" ~ -1*log_odds,
    TRUE ~ log_odds
  )) %>% 
  slice_max(log_odds, n = 8)  %>% 
  mutate(category = fct_reorder(category, log_odds2)) %>%
  ggplot(aes(category, log_odds2, fill = corpus)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Weighted log odds ratio", x = "Stance category", title = "Contrasting stance categories", subtitle = "Calculated by weighted log odds ratios of frequency counts") +
  theme_minimal()



