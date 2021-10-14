## cross referencing id values
library(tidyverse)
## get tribble from excel data call it from_excel ----

reference <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=0&single=true&output=csv")

## get tribble from excel of student_details 

student_details <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=736268226&single=true&output=csv") %>% mutate(student_id = as.integer(student_id))

text_details <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=884469775&single=true&output=csv")%>% mutate(joc_id = str_to_lower(joc_id))

task_details <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=412674908&single=true&output=csv")

comments <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=497333765&single=true&output=csv")


joc_thread_student <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiObx4wEVzMoU0u0gWR83cjLm16qWF2jEIWv3sNQtXQ_mFGW-JsO9Nm4XpfkX5IRwEdNrQpBnrDA1o/pub?gid=2101187864&single=true&output=csv")

student_details %>% write_csv(here::here("data","student_details.csv"))
text_details %>% write_csv(here::here("data","text_details.csv")) %>% janitor::clean_names() 
task_details %>% write_csv(here::here("data","task_details.csv"))
reference %>% write_csv(here::here("data","reference.csv"))

reference %>% inner_join(text_details, by = c("corpus_id" = "joc_id") ) %>% filter(student_id != "Teacher") %>% 
  group_by(topic_id) %>% mutate(n = row_number())

comments %>% write_csv(here::here("data","comments.csv"))

joc_thread_student %>% write_csv(here::here("data","joc_thread_student.csv"))



### code for matching wmatrix and original files by stripped comments


wmatrix_dat <- readRDS("~/Desktop/cmc-all/wmatrix/data/wmatrix_dat.rds")
# 
# strip_comment <- function(x){
#   x %>% tolower() %>% str_remove_all('[[:punct:]]+') %>% str_squish
# }
# 
# stripped <- comments %>% mutate(comment = strip_comment(comment)) %>% arrange(comment)
# 
# wmatrix_dat %>% filter(corpus == "jusoc") %>% unnest(data) %>% 
#   mutate(corpus = "JOC") %>% 
#   group_by(id) %>% 
#   summarise(comment = paste(word, collapse = " ")) %>%
#   mutate(comment = strip_comment(comment)) %>% 
#   filter(str_detect(comment,"in this tragedy victims were chosen at random ")) %>% pull(comment)
# 
# 
# %>% full_join(stripped) %>% arrange(comment) %>% fill(id, .direction = "down") %>% fill(joc_id, .direction = "up") %>% distinct(id,joc_id) %>% arrange(joc_id) %>% inner_join(comments) %>% add_count(name = "joc_n" ,joc_id) %>% add_count(name = "id_n" ,id) %>% 
#  



# joining student and wmatrix details -------------------------------------
joc_stance_count <- readRDS("~/Desktop/cmc-all/concordancing/data/JOC_comment_stance_count.rds")


joined <- joc_thread_student %>% left_join(student_details) %>%
  left_join(text_details) %>% 
right_join(joc_stance_count, by = c("id" = "thread_id"),.) %>% filter(!is.na(student_id))
glimpse(joined)
  
## add topic_id to join with task details
joined <- joined %>% mutate(topic_id = str_remove(id,"\\d{3}$")) %>% left_join(task_details)

## separate into 1st or second semester
joined <- joined %>% mutate(semester = ifelse(collection_period %in% c("I","II","III"), "first", "second"))

glimpse(joined)
  
  ## add counts per 10k
  student_stance_count <- 
  joined %>% group_by(student_id) %>% 
  mutate(student_wc = sum(wc), .after = student_id) %>% 
  ungroup() %>% 
  group_by(student_id, pattern) %>% 
  mutate(pattern_per_10k = sum(count)*10000/student_wc, .after = pattern) %>%
  ungroup() %>% 
  group_by(student_id, subcategory) %>%
  mutate(subcat_per_10k = sum(count)*10000/student_wc, .after = pattern_per_10k) %>%
  ungroup() %>% 
  group_by(student_id, category) %>%
  mutate(category_per_10k = sum(count)*10000/student_wc, .after = subcat_per_10k) %>%
 # select(corpus,student_id,cohort,pattern,subcategory,category,pattern_per_10k,subcat_per_10k,category_per_10k) %>% 
  ungroup()
  

  student_stance_count %>% filter(!is.na(pattern)) %>% 
  filter(category == "modal") %>% 
  mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
  ggplot(aes(pattern,pattern_per_10k,fill = original)) +
  geom_boxplot(position = "dodge") +
  coord_flip() +
  facet_wrap(~subcategory, scales = "free") +
  theme_minimal()  
  
  student_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(category == "stance verb + (that) clause") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = cohort)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
  
  student_stance_count %>% filter(!is.na(pattern)) %>% 
    filter(subcategory == "epistemic verbs - certainty") %>% 
    mutate(pattern = fct_reorder(pattern, pattern_per_10k)) %>% 
    ggplot(aes(pattern,pattern_per_10k,fill = original)) +
    geom_boxplot(position = "dodge") +
    coord_flip() +
    facet_wrap(~subcategory, scales = "free") +
    theme_minimal()  
  


# log odds ----------------------------------------------------------------
  
  library(tidylo)
  
  ## create log odds dataframes for original vs reply

  ## log odds for patterns
  pattern_lo <-    student_stance_count %>% group_by(original,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup %>% 
    bind_log_odds(original, pattern, n) %>% 
    select(original,pattern,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      original=="reply" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  
  
  ## log odds for subcategories
  
  subcat_lo <-    student_stance_count %>% group_by(original,subcategory)  %>% 
    summarise(n = mean(subcat_per_10k)) %>% ungroup %>% 
    bind_log_odds(original, subcategory, n) %>% 
    select(original,subcategory,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      original=="reply" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## log odds for categories
  
  category_lo <-    student_stance_count %>% group_by(original,category)  %>% 
    summarise(n = mean(category_per_10k)) %>% ungroup %>% 
    bind_log_odds(original, category, n) %>% 
    select(original,category,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      original=="reply" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
 ## plotting original vs reply
  
  ## patterns
  pattern_lo %>%
    group_by(original) %>%
    top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
    ggplot(aes(pattern, log_odds2, fill = original)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## subcategories
  subcat_lo %>%
    group_by(original) %>%
    filter(log_odds > 0) %>%
    # top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
    ggplot(aes(subcategory, log_odds2, fill = original)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## categories
  category_lo %>% filter(!is.na(category)) %>% 
    group_by(original) %>%
    filter(log_odds > 0) %>%
    ungroup() %>%
    mutate(category = fct_reorder(category, log_odds2)) %>%
    ggplot(aes(category, log_odds2, fill = original)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  ## create log odds dataframes for news vs class ----
  
  ## log odds for patterns
  pattern_lo <-    student_stance_count %>% group_by(type,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup %>% 
    bind_log_odds(type, pattern, n) %>% 
    select(type,pattern,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      type=="class" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  
  
  ## log odds for subcategories
  
  subcat_lo <-    student_stance_count %>% group_by(type,subcategory)  %>% 
    summarise(n = mean(subcat_per_10k)) %>% ungroup %>% 
    bind_log_odds(type, subcategory, n) %>% 
    select(type,subcategory,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      type=="class" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## log odds for categories
  
  category_lo <-    student_stance_count %>% group_by(type,category)  %>% 
    summarise(n = mean(category_per_10k)) %>% ungroup %>% 
    bind_log_odds(type, category, n) %>% 
    select(type,category,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      type=="class" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## plotting 
  
  ## patterns
  pattern_lo %>%
    group_by(type) %>%
    top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
    ggplot(aes(pattern, log_odds2, fill = type)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## subcategories
  subcat_lo %>%
    group_by(type) %>%
    filter(log_odds > 0) %>%
    # top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
    ggplot(aes(subcategory, log_odds2, fill = type)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## categories
  category_lo %>% filter(!is.na(category)) %>% 
    group_by(type) %>%
    filter(log_odds > 0) %>%
    ungroup() %>%
    mutate(category = fct_reorder(category, log_odds2)) %>%
    ggplot(aes(category, log_odds2, fill = type)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## create log odds dataframes for 1st vs 2nd semester ----
  
  ## log odds for patterns
  pattern_lo <-    student_stance_count %>% group_by(semester,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup %>% 
    bind_log_odds(semester, pattern, n) %>% 
    select(semester,pattern,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      semester=="first" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  
  
  ## log odds for subcategories
  
  subcat_lo <-    student_stance_count %>% group_by(semester,subcategory)  %>% 
    summarise(n = mean(subcat_per_10k)) %>% ungroup %>% 
    bind_log_odds(semester, subcategory, n) %>% 
    select(semester,subcategory,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      semester=="first" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## log odds for categories
  
  category_lo <-    student_stance_count %>% group_by(semester,category)  %>% 
    summarise(n = mean(category_per_10k)) %>% ungroup %>% 
    bind_log_odds(semester, category, n) %>% 
    select(semester,category,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      semester=="first" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## plotting 
  
  ## patterns
  pattern_lo %>%
    group_by(semester) %>%
    top_n(20, log_odds) %>%
    ungroup() %>%
    mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
    ggplot(aes(pattern, log_odds2, fill = semester)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## subcategories
  subcat_lo %>%
    group_by(semester) %>%
    filter(log_odds > 0) %>%
    # top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
    ggplot(aes(subcategory, log_odds2, fill = semester)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## categories
  category_lo %>% filter(!is.na(category)) %>% 
    group_by(semester) %>%
    filter(log_odds > 0) %>%
    ungroup() %>%
    mutate(category = fct_reorder(category, log_odds2)) %>%
    ggplot(aes(category, log_odds2, fill = semester)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()

  
# tidylo for length ---- 
  
  
  ## add length column
  student_stance_count %<>% mutate(length = case_when(
    wc < 50 ~ "short",
    wc > 105 ~ "long",
    TRUE ~ "medium"
  ))
  
  pattern_lo <-     student_stance_count %>% 
    filter(length != "medium") %>% 
 group_by(length,pattern)  %>% 
    summarise(n = mean(pattern_per_10k)) %>% ungroup %>% 
    bind_log_odds(length, pattern, n) %>% 
    select(length,pattern,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      length=="short" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  
  
  ## log odds for subcategories
  
  subcat_lo <-    student_stance_count %>% 
    filter(length != "medium") %>% 
    group_by(length,subcategory)  %>% 
    summarise(n = mean(subcat_per_10k)) %>% ungroup %>% 
    bind_log_odds(length, subcategory, n) %>% 
    select(length,subcategory,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      length=="short" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## log odds for categories
  
  category_lo <-    student_stance_count %>% 
    filter(length != "medium") %>% 
    group_by(length,category)  %>% 
    summarise(n = mean(category_per_10k)) %>% ungroup %>% 
    bind_log_odds(length, category, n) %>% 
    select(length,category,n, log_odds = log_odds_weighted) %>% 
    mutate(log_odds2 = case_when(
      length=="short" ~ -1*log_odds,
      TRUE ~ log_odds
    )) 
  
  ## plotting 
  
  ## patterns
  pattern_lo %>%
    group_by(length) %>%
    top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(pattern = fct_reorder(pattern, log_odds2)) %>%
    ggplot(aes(pattern, log_odds2, fill = length)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## subcategories
  subcat_lo %>%
    group_by(length) %>%
    filter(log_odds > 0) %>%
    # top_n(10, log_odds) %>%
    ungroup() %>%
    mutate(subcategory = fct_reorder(subcategory, log_odds2)) %>%
    ggplot(aes(subcategory, log_odds2, fill = length)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
  
  ## categories
  category_lo %>% filter(!is.na(category)) %>% 
    group_by(length) %>%
    filter(log_odds > 0) %>%
    ungroup() %>%
    mutate(category = fct_reorder(category, log_odds2)) %>%
    ggplot(aes(category, log_odds2, fill = length)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal()
  
    

  
  
  
