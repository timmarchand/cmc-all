
# libraries and data ------------------------------------------------------

# load libraries
pacman::p_load(tidyverse,magrittr)


# get the ngrams from wmatrix folder
C7_ngrams <- readRDS("~/Desktop/cmc-all/wmatrix/data/C7_ngrams.rds")

# filter out BNC corpora, punctuation marks
cmc_pos <-  C7_ngrams %>% filter(corpus %in% c("JOC", "HOC"), lemma != "PUNC", unipos != "...", unipos != ")",  unipos != "(") %>% 
  filter(!str_detect(bipos,get_rid)) %>% 
  filter(!str_detect(bipos,get_rid))


## add word count
cmc_pos  %<>% count(corpus, name = "wc")  %>% 
  left_join(cmc_pos) 

# functions ---------------------------------------------------------------

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
  tbl_1 <- tibble(
    corpus = df %>% colnames %>% nth(2),
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

  tbl_2 <- tibble(
    corpus = df %>% colnames %>% nth(3),
    item = df %>% pull(1),
    c1 = df %>% pull(3),
    c2 = df %>% pull(2),
    c1_sum = df %>% pull(5),
    c2_sum = df %>% pull(4),
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
  tbl_2  %>% 
    bind_rows(tbl_1) %>% 
    select(corpus,item,LL,`%DIFF`,BIC,ELL,rel_risk,log_ratio,odds_ratio,log_odds) 
  
  
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

  

# keyness calculation table table  after Rayson --------------------------------------------------
  
  # calculate totals for each unipos
 rayson_unipos_tbl <-  cmc_pos %>% 
    distinct(corpus, wc, unipos, unicount) %>% 
    group_by(corpus,unipos) %>% 
    summarise(total_uni = sum(unicount)) %>% arrange(unipos) %>% 
    left_join(cmc_pos) %>% 
    distinct(corpus, unipos, total_uni, wc) %>% 
    pivot_wider(id_cols = c(unipos), names_from = c(corpus), values_from = total_uni, values_fill = 0) %>% 
    mutate(HOC_wc = 1538720, JOC_wc = 546699)
 

 # calculate totals for each bipos
 rayson_bipos_tbl <-  cmc_pos %>% 
   distinct(corpus, wc, bipos, bicount) %>% 
   group_by(corpus,bipos) %>% 
   summarise(total_bi = sum(bicount)) %>% arrange(bipos) %>% 
   left_join(cmc_pos) %>% 
   distinct(corpus, bipos, total_bi, wc) %>% 
   pivot_wider(id_cols = c(bipos), names_from = c(corpus), values_from = total_bi, values_fill = 0) %>% 
   mutate(HOC_wc = 1538720, JOC_wc = 546699) %>% 
   # set minimum frequency
   filter(HOC + JOC > 1000) %>% 
   I()
 
 cmc_pos %>% count(bipos, sort = TRUE) %>% 
   skimr::
 
 # calculate totals for each tripos
 rayson_tripos_tbl <-  cmc_pos %>% 
   distinct(corpus, wc, tripos, tricount) %>% 
   group_by(corpus,tripos) %>% 
   summarise(total_tri = sum(tricount)) %>% arrange(tripos) %>% 
   left_join(cmc_pos) %>% 
   distinct(corpus, tripos, total_tri, wc) %>% 
   pivot_wider(id_cols = c(tripos), names_from = c(corpus), values_from = total_tri, values_fill = 0) %>% 
   mutate(HOC_wc = 1538720, JOC_wc = 546699) %>% filter(HOC + JOC > 10)

 
## Plotting
 
 
 ## bipos
 rayson_bipos_tbl %>% 
key_tbl %>% 
   filter(corpus =="HOC") %>% 
   mutate(abs= abs(log_odds)) %>% 
   mutate(pos = log_odds <= 0) %>% 
  slice_max(abs, n = 100)  %>% 
   ungroup() %>%
   mutate(item = fct_reorder(item, log_odds)) %>%
   ggplot(aes(item, log_odds, fill = pos)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   theme_minimal()
 
 ## tripos
 rayson_tripos_tbl %>% 
   key_tbl %>% 
   filter(corpus =="HOC") %>% 
   mutate(abs= abs(log_odds)) %>% 
   mutate(pos = log_odds <= 0) %>% 
   slice_max(abs, n = 100)  %>% 
   ungroup() %>%
   mutate(item = fct_reorder(item, log_odds)) %>%
   ggplot(aes(item, log_odds, fill = pos)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   theme_minimal()
 
 
## keyness calculation for weighted log odds-----------
 
 # calculate totals for each unipos
 tidylo_unipos_tbl <-  cmc_pos %>% 
   distinct(corpus, wc, unipos, bicount) %>% 
   group_by(corpus,bipos) %>% 
   summarise(n = sum(unicount)) %>% arrange(unipos) %>% 
   left_join(cmc_pos) %>% 
   distinct(corpus, unipos, n)
 
 
 # calculate totals for each bipos
 tidylo_bipos_tbl <-  cmc_pos %>% 
   distinct(corpus, wc, bipos, bicount) %>% 
   group_by(corpus,bipos) %>% 
   summarise(n = sum(bicount)) %>% arrange(bipos) %>% 
   left_join(cmc_pos) %>% 
   distinct(corpus, bipos, n)

 # calculate totals for each tripos
 tidylo_tripos_tbl <-  cmc_pos %>% 
   distinct(corpus, wc, tripos, tricount) %>% 
   group_by(corpus,tripos) %>% 
   summarise(n = sum(tricount)) %>% arrange(tripos) %>% 
   left_join(cmc_pos) %>% 
   distinct(corpus, tripos, n)
 
 
 ## plotting
 
 tidylo_bipos_tbl %>% 
   rename(item = bipos) %>% 
   bind_log_odds(set = corpus, feature = item, n = n) %>% 
   select(corpus ,item,n, log_odds = log_odds_weighted) %>% 
   filter(item != "other") %>% 
   mutate(log_odds2 = case_when(
     corpus =="JOC" ~ -1*log_odds,
     TRUE ~ log_odds
   )) %>% 
   slice_max(log_odds, n = 50)  %>% 
   ungroup() %>%
   mutate(item = fct_reorder(item, log_odds2)) %>%
   ggplot(aes(item, log_odds2, fill = corpus)) +
   geom_col(show.legend = FALSE) +
   coord_flip() +
   theme_minimal()
 
 
 tl3 <- tidylo_tripos_tbl %>% 
   rename(item = tripos) %>% 
   bind_log_odds(set = corpus, feature = item, n = n) %>% 
   select(corpus ,item,n, log_odds = log_odds_weighted) %>% 
   filter(item != "other") %>% 
   mutate(log_odds2 = case_when(
     corpus =="JOC" ~ -1*log_odds,
     TRUE ~ log_odds
   )) %>% hist(log_odds)
 
 
 tidylo_tripos_tbl %>% 
   rename(item = tripos) %>% 
   bind_log_odds(set = corpus, feature = item, n = n) %>% 
   select(corpus ,item,n, log_odds = log_odds_weighted) %>% 
   filter(item != "other") %>% 
   mutate(log_odds2 = case_when(
     corpus =="JOC" ~ -1*log_odds,
     TRUE ~ log_odds
   )) %>% 
  # filter(between(log_odds, -0.15,0.15) ) %>% 
   slice_max(log_odds, n = 5)  %>% 
   ungroup() %>%
   mutate(item = fct_reorder(item, log_odds2)) %>%
   ggplot(aes(item, log_odds2, fill = corpus)) +
   geom_col(show.legend = TRUE) +
   coord_flip() +
   labs(y = "Weighted log odds ratio", x = "PoS trigram", title = "Top 5 contrasting PoS trigrams", subtitle = "Calculated by weighted log odds ratios of frequency counts") +
   theme_minimal()
 
 
 tidylo_tripos_tbl %>% 
   rename(item = tripos) %>% 
   bind_log_odds(set = corpus, feature = item, n = n) %>% 
   select(corpus ,item,n, log_odds = log_odds_weighted) %>% 
   filter(item != "other") %>% 
   mutate(log_odds2 = case_when(
     corpus =="JOC" ~ -1*log_odds,
     TRUE ~ log_odds
   )) %>% 
   # filter(between(log_odds, -0.15,0.15) ) %>% 
   slice_max(log_odds, n = 5)  %>% 
   ungroup() %>%
   mutate(item = fct_reorder(item, log_odds2)) %>%
   ggplot(aes(item, log_odds2, fill = corpus)) +
   geom_col(show.legend = TRUE) +
   coord_flip() +
   labs(y = "Weighted log odds ratio", x = "PoS trigram", title = "Top 5 contrasting PoS trigrams", subtitle = "Calculated by weighted log odds ratios of frequency counts") +
   theme_minimal()
 
 
 tidylo_tripos_tbl %>% 
   rename(item = tripos) %>% 
   bind_log_odds(set = corpus, feature = item, n = n) %>% 
   ungroup() %>% 
   filter(between(log_odds_weighted, -0.05,0.05)) %>% arrange(abs(log_odds_weighted))
   
 plogis(0.05)
  ## visualising text ----
 
 string <- "II_AT_NP1"
 
 ## full text
 
tripos1 <- cmc_pos %>% filter(tripos == string) %>% count(corpus,trigram, sort = TRUE) %>% left_join(cmc_pos) %>% 
   mutate(per_10k = 10000*n/wc) %>% distinct(corpus,trigram, per_10k) %>% 
   #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,per_10k, label = trigram, color = corpus)) +
   geom_text(position=position_jitter(width=0.2,height=0), check_overlap = FALSE,angle = 45 ) +
   coord_flip() +
   theme_minimal() +
   easy_remove_legend() +
  labs(y = "", title = 'Trigram frequency of the pattern II_AT_NP1')
 

## picking out some examples
tripos2 <-  cmc_pos %>% filter(tripos == string) %>% count(corpus,trigram, sort = TRUE) %>% left_join(cmc_pos) %>% 
 mutate(per_10k = 10000*n/wc) %>% distinct(corpus,trigram, per_10k) %>% 
 #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,per_10k, label = trigram, color = corpus)) +
   geom_text(position=position_jitter(width=0.2,height=0), check_overlap = TRUE,angle = 45 ) +
  coord_flip() +
   theme_minimal() +
   easy_remove_legend()  +
   labs(y = "Frequency per 10,000 tokens")
 
 
grid.arrange(tripos1,tripos2, nrow = 2)
  string <- "II_AT_NP1"
 ## full text
 ## picking out some examples
 cmc_pos %>% filter(tripos == string) %>% count(corpus,trigram, sort = TRUE) %>% left_join(cmc_pos) %>% 
   mutate(per_10k = 10000*n/wc) %>% distinct(corpus,trigram, per_10k) %>% 
   #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,per_10k, label = trigram, color = corpus)) +
   geom_text(position=position_jitter(width=0.2,height=0), check_overlap = FALSE,angle = 45 ) +
   coord_flip() +
   theme_minimal() +
   easy_remove_legend()
 
 
 ## picking out some examples
 cmc_pos %>% filter(tripos == string) %>% count(corpus,trigram, sort = TRUE) %>% left_join(cmc_pos) %>% 
   mutate(per_10k = 10000*n/wc) %>% distinct(corpus,trigram, per_10k) %>% 
   #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,per_10k, label = trigram, color = corpus)) +
   geom_text(position=position_jitter(width=0.2,height=0), check_overlap = TRUE,angle = 45 ) +
   coord_flip() +
   theme_minimal() +
   easy_remove_legend()
 
 
 
### try with factor tags
 C7_mode <- C7_ngrams %>%  
   mutate(mode = ifelse(corpus == "DEM","SPK",
                                     ifelse(corpus %in% c("HOC","JOC"),"CMC","WTN")), 
                       .before = corpus) %>% 
    mutate(corpus = factor(corpus,levels = c("ACA","NWS","FIC","HOC","JOC","DEM")))   
 
 # dput(Dim1)
 # c("JJ_NN1_IO", "AT_NN1_IO", "AT_JJ_NN1", "NN1_IO_AT", "IO_AT_JJ", 
 #   "NN1_IO_JJ", "JJ_NN1_II", "VVN_II_AT", "JJ_NN2_IO", "II_AT_JJ", 
 #   "IO_JJ_NN1", "IO_AT_NN1", "NN1_II_AT", "AT1_JJ_NN1", "AT_JJ_NN2", 
 #   "NN1_IO_NN1", "II_JJ_NN2", "II_AT1_JJ", "JJ_JJ_NN1", "NN1_CC_NN1", 
 #   "NN1_II_NN1", "NN1_IO_NN2", "II_AT1_NN1", "JJ_NN1_NN1", "AT_NN1_II", 
 #   "II_AT_NN2", "AT1_NN1_IO", "VM_VBI_VVN", "AT_NN1_NN1")
 # 
 string <- "NN2_II_AT"
 
 ## full text
 C7_mode %>% 
   filter(tripos == string) %>% count(mode, corpus,trigram, sort = TRUE) %>% 
 #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,n, label = trigram) ) +
   geom_text(position=position_jitter(width=0.2,height=1), check_overlap = FALSE,angle = 45, alpha = 0.3) +
   coord_flip() +
   theme_minimal() +
   easy_remove_legend()
 
 
 ## picking out some examples

 C7_mode %>% filter(tripos == string) %>% count(mode,corpus,trigram, sort = TRUE) %>% 
 #  filter(!str_detect(trigram,"_news|article")) %>% 
   ggplot(aes(corpus,n, label = trigram, color = mode)) +
   geom_text(position=position_jitter(width=0.2,height=1), check_overlap = TRUE,angle = 45 ) +
   coord_flip() +
   theme_minimal() +
   easy_remove_legend() 
 
 
