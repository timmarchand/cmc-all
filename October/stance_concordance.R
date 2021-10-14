
## extracting total numbers  of regex matches ####

# clear memory
rm(list=ls(all=TRUE)) 
# libraries
pacman::p_load(tidyverse, gsubfn,here, magrittr, tidylo)
set.seed(4)


joc_threads %>% pluck("full_tags", 1) -> qwe

str_extract_all(qwe,"_could_[^_ ]+")

# functions

# paste together tokens with the or pipe
paste_OR <- function(...){
  input <- list(...)
  items <- paste(sapply(input,paste,collapse = " "),collapse ="|")
  return(paste0("(",items,")"))
}

# paste together tokens with a white space
paste_AND<- function(...){
  input <- list(...)
  items <- paste(sapply(input,paste,collapse = " "),collapse = "\\s")
  return(paste0("(",items,")"))
}

# paste an optional marker
optional <- function(regex){
  return(paste0("(",regex,"\\s)?"))
}

search_df %>% pluck("regex",2) -> asd

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
anyThat <- "(([^_ ])+_CST_that)"
anyPrep <- "(([^_ ])+_I[^_ ]_([^_ ])+)"
anyDet <- paste_OR(anyArt,anyDemP)
anySimpNounPhrase <- paste0(optional(anyDet),anyNoun,anySpace,anyPrep,anySpace,optional(anyDet),anyNoun)
anyCompNounPhrase <- paste0(optional(paste0(paste_OR(anyAdj,anyAdv,anyDet,anyPossAdj),anySpace)),optional(anyAdj),anyNoun,anySpace,anyPrep,optional(paste0(anySpace,paste_OR(anyDet,anyDemP))),anySpace,anyNoun)


case

# that deletion cases
## note when building long regexes, anySpace required between regex patterns
## except after optional() where space is already embedded

## case 1: match foll0wed by demonstrative pronoun or subject pronoun 
case_1 <- paste_OR(anyDemP,anySubjPro)

# case 2: match followed by any pronoun or noun followed by any verb form
case_2 <- paste0(paste_OR(anyPron,anyNoun),anySpace,anyVerb)

# case 3: match followed by an adjective (JJ or PRED), an adverb (RB), a determiner (DT, QUAN, CD) 
# or a possessive pronoun (PRP$) and then a noun (N) and then a verb or auxiliary verb, 
# with the possibility of an intervening adjective (JJ or PRED) between the noun and its preceding word.

case_3 <- paste0(paste_OR(anyAdj,anyAdv,anyDet,anyPossAdj),anySpace,optional(anyAdj),anyNoun,anySpace,anyVerb)

## case 4: match followed by noun, then preposition, then optional determiner, then noun, then verb

case_4 <- paste0(anySimpNounPhrase,anySpace,anyVerb)



## apply cases to search_df

search_df2 <- search_df %>% #filter(str_detect(pattern, "(that)")) %>% 
  mutate(regex = ifelse((str_detect(pattern, "(that)")),paste_OR(regex,anyThat,case_1,case_2,case_3,case_4), regex))

# folders
input_folder <- "/Users/Tim/Downloads/

"
output <- "/Users/Tim/Downloads/stance_output"
mainFoldr<-dir(input_folder) # get the subfolder names
###setup our directory 
subfolders<-paste0(input_folder,"/",mainFoldr)

#read in the files
search_df <- read_delim(here("concordancing","data","stanceRegex4.txt"), delim ="\t")
files <- list.files(input_folder,pattern = ".txt",full.names = TRUE)
file_list <- vector("list", length(mainFoldr))
search_list <- vector("list", nrow(search_df))


# alternative - read in from C7_df

## read in wmatrix C7 tags
C7_df <- readRDS("~/Desktop/cmc-all/wmatrix/data/C7_df.rds")
 
C7_df %>% filter(corpus == "JOC")
cmc_tags <- 
  C7_df %>%  
  # filter only CMC corpora
  filter(corpus == "JOC"  | corpus == "HOC") %>% 
  # join tags with underscores
    mutate(tagged = paste(word,C7,lemma, sep = "_")) %>% 
  # paste and collapse for each thread id
    group_by(corpus, id) %>% 
    summarise(tagged_thread = paste(tagged, collapse = " " )) 


# regex searches to be used
searches <- search_df$regex
file_list <- vector("list", nrow(cmc_tags))
search_list <- vector("list", nrow(search_df))

# for (i in 1:length(subfolders)) {
#   
#   # read in each file as a text
#   text <- read_delim(files[i], delim ="\t",col_names = FALSE)
#   # create text_id variable from file name
#   text_id <- tools::file_path_sans_ext(basename(files[i]))
#  # create corpus name by grabbing the first 3 letters of text_id
#    corpus <- substr(text_id, 1, 3)  
#    # print progress status
#    cat(paste0("\n","matching searches in ", text_id,"\t", round(i*100/length(subfolders),2),"% completed","\n"))
   
for (i in 1:nrow(cmc_tags)) {

  
     # read in each file as a text
     text <- cmc_tags$tagged_thread[i]
     # create text_id variable from file name
     text_id <- cmc_tags$id[i]
     # create corpus name by grabbing the first 3 letters of text_id
     corpus <- cmc_tags$corpus[i]
#      # print progress status
    cat(paste0("\n","matching searches in ", text_id,"\t", round(i*100/nrow(cmc_tags),2),"% completed","\n"))
     

for (k in 1:length(searches)){
  
 
  # alternative progress update
# cat(paste0("\n","matching ",toupper(search_df$pattern[k]), " in ", text_id))
  
  # see if pattern requires that clause
  if(str_detect(search_df$pattern[k],"(that)")==1)
    # if that clause required, check there is that or that deletion clause, go to next search if not
      if(str_detect(text, paste0(searches[k],paste_OR(anyThat,case_1,case_2,case_3,case_4)))==0){next}
    # locate matches with that and that deletion (combining 4 cases)  
  else locate <- as.data.frame(str_locate_all(text[[1]],
                      paste0(searches[k],paste_OR(anyThat,case_1,case_2,case_3,case_4))))
    else # locate matches of other searches
      locate <- as.data.frame(str_locate_all(text[[1]],searches[k]))
    
    # check the span to the left side of match - if shorter than 300 charcters, start left extract from text beginning     
     if(min(locate[[1]])<300) left_starts <- 1
       else left_starts <- as.vector(locate[[1]]-300)
      # end left extract one character before match location starts
        left_ends <- as.vector(locate[[1]]-1)
        # start right side extract one character after match location ends
        right_starts <- as.vector(locate[[2]]+1)
        # check the number of characters to the right side of match - if shorter than 300 charcters, end right extract at text end  
         if(nchar(text)-max(locate[[2]])<300) right_ends <- nchar(text)
        else right_ends <- as.vector(locate[[2]]+300)
  # define extracts by location  
  match_extract <- str_sub(text,locate[[1]],locate[[2]])
  left_extract <- str_sub(text,left_starts,left_ends)
  right_extract <- str_sub(text,right_starts,right_ends)
  # define varaibles for df
  bib <- search_df$Biber[k]
  cat <- search_df$category[k]
  subcat <- search_df$subcategory[k]
  pat <- search_df$pattern[k]
  wc <- str_count(text,anyWord)
  
  # remove the tags
  left_text <- gsubfn("([^_]+)(_[^_]+_[^_]+)(\\s)","\\1\\3",x=left_extract,perl = TRUE)
  right_text <- gsubfn("([^_]+)(_[^_]+_[^_]+)(\\s)","\\1\\3",x=right_extract,perl = TRUE)
  match_text <- gsubfn("([^_]+)(_[^_]+_[^_]+)(\\s)","\\1\\3",x=match_extract,perl = TRUE)
  
  # reset extract_all
  extract_all <- NULL
  # create dataframe - note cbind coerces some variables into factors or character strings
  extract_all <- as.data.frame(cbind(corpus = corpus,
                                     text_id = text_id,
                                     Biber = bib,
                                     category = cat,
                                     subcategory = subcat,
                                     pattern = pat,
                                     left_tags = left_extract,
                                     match_tags = match_extract, 
                                     right_tags = right_extract,
                                     left_text = left_text,
                                     match_text = match_text, 
                                     right_text = right_text))
  # add column of # of each pattern match within each text file
  extract_all$pattern_match_count <- seq_along(extract_all[,1])
  # add column of total # of each pattern match for each text file
  extract_all$pattern_match_total <- nrow(extract_all)
  # add column of proportions per 100000 words
  extract_all$pattern_match_per_10k <- 10000*(extract_all$total_match_count/wc)
  # add column of word count for each text
  extract_all$word_count <- wc
  
  # place extract into subsequent slot in the list of searches
  search_list[[k]] <- extract_all
  }

   # place list of searches into subsequent slot in the list of text files
file_list[[i]] <- search_list
  
}

names(asd)

final_df5 %<>% rename(pattern_match_total = total_match_count,
               pattern_match_per_10k = match_per_10k)

## get the total counts for each category and subcategory per text and per 10k words
final_df5 %<>% add_count(text_id, category, name = "category_match_total") %>% 
  add_count(text_id, subcategory, name = "subcategory_match_total") %>% 
  mutate(category_match_per_10k = 10000*(category_match_total/word_count)) %>% 
  mutate(subcategory_match_per_10k = 10000*(subcategory_match_total/word_count)) 
        





# unlist all the data to put into dataframe
final_list <- unlist(file_list, recursive = FALSE)   

# bind all dfs together into one df
final_df5 <- as_tibble(do.call(bind_rows,final_list))

final_df5 %<>% filter(!is.na(match_tags))

   
# write output file   
    final_df %>% write_csv(here("concordancing","data","concordance_output.csv"))


   
    
     asd %<>% distinct(corpus,text_id,word_count) %>% 
      mutate(corpus_texts = n(),corpus_wc = sum(word_count)) %>% 
      # join to df %>% 
     left_join(asd) %>% 
     group_by(corpus,pattern) %>% 
     mutate(corpus_pattern_total = sum(total_match_count)) %>% 
     ungroup() %>% 
     group_by(corpus,category) %>% 
     mutate(corpus_category_total = sum(category_match_total)                                            ) %>% 
     ungroup() %>% 
     group_by(corpus,subcategory) %>% 
     mutate(corpus_subcategory_total = sum(subcategory_match_total)) %>% 
     ungroup() 
    
     
     final_df5 %<>%   group_by(corpus) %>% distinct(corpus,text_id,word_count) %>% 
       summarise(corpus,corpus_texts = n(),corpus_tag_count = sum(word_count)) %>% distinct() %>% 
       left_join(final_df5,.) %>% 
       group_by(corpus,pattern) %>% 
       mutate(corpus_pattern_total = n()) %>% 
       ungroup() %>% 
       group_by(corpus,category) %>% 
       mutate(corpus_category_total = n()) %>% 
       ungroup() %>% 
       group_by(corpus,subcategory) %>% 
       mutate(corpus_subcategory_total = n()) %>%
       ungroup() %>% 
       mutate(corpus_pattern_per_10k = 10000*(corpus_pattern_total/corpus_tag_count) ) %>% 
       mutate(corpus_category_per_10k = 10000*(corpus_category_total/corpus_tag_count) ) %>% 
       mutate(corpus_subcategory_per_10k = 10000*(corpus_subcategory_total/corpus_tag_count) ) 
     
sub_cat_comparison <- 
       final_df5 %>% 
       distinct(corpus,subcategory,corpus_subcategory_per_10k) %>% pivot_wider(names_from = corpus, values_from = corpus_subcategory_per_10k)

cat_comparison <- 
  final_df5 %>% 
  distinct(corpus,category,corpus_category_per_10k) %>% pivot_wider(names_from = corpus, values_from = corpus_category_per_10k)

pattern_comparison <- 
  final_df5 %>% 
  distinct(corpus,pattern,corpus_pattern_per_10k) %>% pivot_wider(names_from = corpus, values_from = corpus_pattern_per_10k)


sub_cat_comparison %>% mutate(ratio = (HOC)/(JOC)) %>% arrange(-ratio)

cat_comparison %>% mutate(ratio = (HOC)/(JOC)) %>% arrange(-ratio)

pattern_comparison %>% mutate(ratio = (HOC)/(JOC)) %>% arrange(-ratio)

final_df5 %>% 
  distinct(corpus,category, pattern,corpus_pattern_per_10k) -> patterns

patterns %>% count(Biber)

patterns %>% #filter(category == "stance verb + to-clause") %>% 
  mutate(pattern = fct_reorder(pattern, corpus_pattern_per_10k)) %>% 
  ggplot(aes(pattern,corpus_pattern_per_10k, fill = corpus)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_light()
  


library(tidylo)

book_words %>% 
  bind_log_odds(set = book, feature = word, n = n) %>%
  arrange(desc(log_odds))

       
     
##############################################################################    
################## summary data and reload from file ########################
   library(tidyverse) 
    set.seed(4)
    # read in file
  final_df2 <- read_csv(here("concordancing","data","concordance_output.csv"))
  
  final_df %>% distinct(text_id,pattern,match_per_10k)
  
  # add random numbers to final_df data
  final_df$sample <- sample(size = nrow(final_df),replace = FALSE,1:nrow(final_df))
  
  list <- split(final_df, final_df$corpus)
  
  HYC <- list[[1]]
  
  JUC <- list[[2]]

  ######### JUC category ##########
  
# derive the number of words in each corpus by adding all distinct text_id and word_count value

  # add random numbers to JUC data
   JUC$JUC_sample <- sample(size = nrow(JUC),replace = FALSE,1:nrow(JUC))
  
 JUC %<>%
  distinct(corpus,text_id,word_count) %>% 
  mutate(corpus_texts = n(),corpus_wc = sum(word_count)) %>% 
   # join to df
   left_join(JUC)

  
 # derive the number category matches in each text by grouping by text_id and adding  total match_count values
 JUC %>%
   distinct(text_id,category,pattern,total_match_count) %>% 
   group_by(text_id,category) %>% 
   summarise(total_category_in_text = sum(total_match_count))  %>% 
   group_by(category) %>% 
   summarise(total_category_in_corpus = sum(total_category_in_text))  %>% 
   inner_join(JUC,.)
 # 
 # # join to df
 # final_df <- left_join(final_df,asd)
 
 JUC %>% 
   distinct(corpus_wc) -> zxc
  corpus_wc <- zxc[[1]]

  (asd$category_propn  <-  round(asd$sum_corp_pat*10000/corpus_wc))
 

JUC <- left_join(JUC,asd)

######### JUC subcategory ##########

# derive the number of words in each corpus by adding all distinct text_id and word_count value


# derive the number subcategory matches in each text by grouping by text_id and adding  total match_count values
JUC %>%
  distinct(corpus,text_id,category,subcategory,pattern,total_match_count) %>% 
  group_by(corpus,text_id,category,subcategory) %>% 
  summarise(sum_text_sub = sum(total_match_count))   %>% 
  group_by(category,subcategory) %>% 
  summarise(sum_corp_sub = sum(sum_text_sub)) %>% 
  arrange(desc(sum_corp_sub)) -> asd
# 
# # join to df
# final_df <- left_join(final_df,asd)

JUC %>% 
  distinct(corpus_wc) -> zxc
corpus_wc <- zxc[[1]]
## proportion values per 100,000
(asd$subcat_propn  <-  round(asd$sum_corp_sub*100000/corpus_wc))


JUC <- left_join(JUC,asd)

JUC %>% head(20) %>% View

##### HYC  ###########

# add random numbers to HYC data
HYC$HYC_sample <- sample(size = nrow(HYC),replace = FALSE,1:nrow(HYC))

HYC %>%
  distinct(corpus,text_id,word_count) %>% 
  mutate(corpus_texts = n(),corpus_wc = sum(word_count)) -> qwe

# join to df
HYC <- left_join(HYC,qwe)

# derive the number category matches in each text by grouping by text_id and adding  total match_count values
HYC %>%
  distinct(text_id,category,pattern,total_match_count) %>% 
  group_by(text_id,category) %>% 
  summarise(sum_text_cat = sum(total_match_count))  %>% 
  group_by(category) %>% 
  summarise(sum_corp_pat = sum(sum_text_cat)) -> asd
# 
# # join to df
# final_df <- left_join(final_df,asd)

HYC %>% 
  distinct(corpus_wc) -> zxc
corpus_wc <- zxc[[1]]

(asd$category_propn  <-  round(asd$sum_corp_pat*10000/corpus_wc))


HYC <- left_join(HYC,asd)

#### subcategory HYC ####
HYC %>%
  distinct(corpus,text_id,category,subcategory,pattern,total_match_count) %>% 
  group_by(corpus,text_id,category,subcategory) %>% 
  summarise(sum_text_sub = sum(total_match_count))   %>% 
  group_by(category,subcategory) %>% 
  summarise(sum_corp_sub = sum(sum_text_sub)) %>% 
  arrange(desc(sum_corp_sub)) -> asd
# 
# # join to df
# final_df <- left_join(final_df,asd)

HYC %>% 
  distinct(corpus_wc) -> zxc
corpus_wc <- zxc[[1]]

## proportion values per 100,000
(asd$subcat_propn  <-  round(asd$sum_corp_sub*100000/corpus_wc))


HYC <- left_join(HYC,asd)



################### combined again category  / subcategory #####

bind_rows(JUC,HYC) -> df

df %>% 
  distinct(corpus,category,category_propn) -> view_cat

df %>% 
  distinct(corpus,category,subcategory,subcat_propn) -> view_sub

view_cat %>% ggplot(aes(category,category_propn, fill = corpus)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_light()

view_sub %>% mutate(subcategory = factor(subcategory)) %>% 
  mutate(category = factor(category)) %>% 
  ggplot(aes(subcategory,subcat_propn, group = category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~category, scales = "free_x") +
  theme_light()


######### JUC pattern  ##########

# derive the number of words in each corpus by adding all distinct text_id and word_count value



# derive the number category matches in each text by grouping by text_id and adding  total match_count values
JUC %>%
  distinct(corpus,text_id,category,pattern,total_match_count) %>% 
  group_by(corpus,text_id,category,pattern) %>% 
  summarise(sum_text_pat = sum(total_match_count))  %>% 
  group_by(corpus,pattern) %>% 
  summarise(sum_corp_pat = sum(sum_text_pat)) %>% 
  arrange(desc(sum_corp_pat)) -> asd
# 
# # join to df
# final_df <- left_join(final_df,asd)


JUC %>% 
  distinct(corpus_wc) -> zxc
corpus_wc <- zxc[[1]]

(asd$pattern_propn  <-  round(asd$sum_corp_pat*1000000/corpus_wc))

asd <- asd %>% select(-sum_corp_pat)

JUC <- left_join(JUC,asd)

glimpse(JUC)

##### HYC  pattern###########
  
# derive the number category matches in each text by grouping by text_id and adding  total match_count values
HYC %>%
  distinct(corpus,text_id,category,pattern,total_match_count) %>% 
  group_by(corpus,text_id,category,pattern) %>% 
  summarise(sum_text_pat = sum(total_match_count))  %>% 
  group_by(corpus,pattern) %>% 
  summarise(sum_corp_pat = sum(sum_text_pat)) %>% 
  arrange(desc(sum_corp_pat)) -> asd
# 
# # join to df
# final_df <- left_join(final_df,asd)


HYC %>% 
  distinct(corpus_wc) -> zxc
corpus_wc <- zxc[[1]]

(asd$pattern_propn  <-  round(asd$sum_corp_pat*1000000/corpus_wc))

asd <- asd %>% select(-sum_corp_pat)

HYC <- left_join(HYC,asd)

glimpse(HYC)

################### combined again pattern #####

bind_rows(JUC,HYC) -> df

df %>% 
  distinct(corpus,pattern,pattern_propn) -> view_pat


######## pivot wider  category NOTE: freq per 10,000 words  #########

# pivot cat data with names of category becoming the new columns
cat_pivot <- view_cat %>% 
  pivot_wider(id_cols = corpus,names_from = category,values_from = category_propn) 

# transpose to make it easier to read
as.data.frame(t(cat_pivot)) -> trans

colnames(trans) <- c("JUC","HYC")
# coerced to factor, so change values to numeric
trans$JUC <- as.numeric(as.character(trans$JUC))
trans$HYC <- as.numeric(as.character(trans$HYC))

# remove the first row (same as column names)
cat_table <- trans[-1,]



cat_table$ratio <- round(cat_table$JUC/cat_table$HYC,2)

cat_table <- rownames_to_column(cat_table, var = "category")
colnames(cat_table) <- c("stance category","JUC","HYC","ratio")

cat_table %>% arrange(desc(ratio))
cat_table %>% arrange(ratio)

### generate subcategory table NOTE: freq per 100,000 words ####

sub_pivot <- view_sub %>% 
  pivot_wider(id_cols = corpus,names_from = subcategory,values_from = subcat_propn) 

# transpose to make it easier to read
as.data.frame(t(sub_pivot)) -> trans

colnames(trans) <- c("JUC","HYC")

# coerced to factor, so change values to numeric
trans$JUC <- as.numeric(as.character(trans$JUC))
trans$HYC <- as.numeric(as.character(trans$HYC))

# remove the first row (same as column names)
sub_table <- trans[-1,]

# change NAs to zero ## not necessary
sub_table[is.na(sub_table)] <- 0

sub_table$ratio <- round(sub_table$JUC/sub_table$HYC,2)

sub_table <- rownames_to_column(sub_table, var = "subcategory")

sub_table %>% arrange(desc(ratio)) %>% tibble()
sub_table %>% arrange(ratio) %>% tibble()


### generate pattern table NOTE: freq per million words ####

pat_pivot <- view_pat %>% 
  pivot_wider(id_cols = corpus,names_from = pattern,values_from = pattern_propn) 

# transpose to make it easier to read
as.data.frame(t(pat_pivot)) -> trans

colnames(trans) <- c("JUC","HYC")

# coerced to factor, so change values to numeric
trans$JUC <- as.numeric(as.character(trans$JUC))
trans$HYC <- as.numeric(as.character(trans$HYC))

# remove the first row (same as column names)
pat_table <- trans[-1,]

# change NAs to zero
pat_table[is.na(pat_table)] <- 0

pat_table$ratio <- round(pat_table$JUC/pat_table$HYC,2)

pat_table <- rownames_to_column(pat_table, var = "pattern")

pat_table %>% arrange(desc(ratio)) %>% tibble() %>% print (n = 100)
pat_table %>% arrange(ratio,desc(HYC)) %>% tibble() %>% print (n = 100)
                                             
