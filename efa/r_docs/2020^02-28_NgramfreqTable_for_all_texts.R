## this scripts takes a folder (or folders) of pos only tagged files as input
## generated after choosing a wordcount cut
## NOTE: wordcount cut needs to be entered for output freq_table to save
# clear all
rm(list=ls(all=TRUE))

# load libraries
library(gsubfn)
library(tools)
library(tidyverse)
library(ngram)
library(data.table)
# input_folder <- "/Users/Tim/Downloads/EFA_select/output/EFA"
output <- "./data/output"
# today <- paste0("/",Sys.Date(),"_")

### using select_df from pos_file_selection_script ####
## choose only texts over 1000 words
pos_for_efa<- read_csv("./data/2020-02-26_pos_for_efa_selection.csv")
pos_for_efa <- pos_for_efa %>% filter(wordcount >999)
pos_for_efa <- select_df
## read previous full tables here ####
##  freq_table <- read_csv("./data/2020-02-26_full_freq_table_no_cut.csv")
# freq_table <- fread(file = file.choose())
# pivot_table <- fread(file = file.choose())
v <- list()

# create empty object for combining phrasetables
freq_table <- NULL
n <- 3 # set the size of the ngrams

dl  <- lapply(pos_for_efa$pos,"[")
# grab the list names
ID <- pos_for_efa$text_id 

  dl2 <-  lapply(dl,function(x){
    y <- names(x)
    text <- as.character((x))
  ng <- ngram(text,n)
  ptab <- as_tibble(get.phrasetable(ng))
  return(ptab)
  # ptab <- ptab %>% select(file_id, ngrams, prop,-freq)
})
  # this code is necessary to grab the list names and add them to each df
  dl2 <- mapply(cbind,dl2,"file_id" = ID, SIMPLIFY = FALSE)

 freq_table <- bind_rows(dl2, do.call(bind_rows,dl2))


# convert proportion to number per thousand
freq_table$prop <- freq_table$prop*1000
pre <- length(unique(freq_table$ngrams))
cat(paste0("there are ",length(unique(freq_table$ngrams))," ",n,"-grams in this table"))

#### with >600 tokens threshold, 235,837 POS trigrams ####
## drop pos trigrams crossing punctuation marks
freq_table[- grep("PUNC", freq_table$ngrams),] -> freq_table
freq_table[- grep("[^A-Z0-9 ]", freq_table$ngrams),] -> freq_table
post <- length(unique(freq_table$ngrams))
cat(paste0("there are ",length(unique(freq_table$ngrams))," ",n, "-grams in this table", "\n", 
           "after dropping ", pre - post, " ngrams with PUNC tags" ))

##  now 217,392 POS trigrams remain

## here to rerun code and produce freq table ####

freq_table %>% filter(prop > 100) -> test ## this just filters texts with few pos trigrams from news and dem

# convert freq_table to a term freq matrix with pivot_wider
## this leaves 1655 rows (corpus files) of 235838 columns (pos trigrams) without filtering

## directly load data from file selection (also in EFA project folder)
#freq_table <-file_df ## this is for full files
#freq_table <- pos_for_efa ## this is for wordcount threshold files
freq_table <- read_csv(file.choose())

# add corpus to freq_table >> maybe unnecessary depending on source of data
freq_table$corpus <- gsubfn("([A-Z]{3}).*","\\1",freq_table$file_id,perl=TRUE)



# # this obtains the frequency totals of each ngram in each corpus
# freq_table %>% group_by(corpus,ngrams) %>% 
#   summarise(total = sum(freq)) -> freq_sums
# # this tallies the total number of rows for each corpus
# freq_table %>% group_by(corpus) %>% tally() -> corp_sum
# # this creates a table of the proportion of each ngram within each corpus per thousand
# left_join(freq_sums,corp_sum) %>% 
#   mutate(corp_prop = total / n) -> corp_prop
# 
#   corp_prop$per_k <- corp_prop$total*1000 / corp_prop$n 
#   
#   corp_prop %>% filter(corpus == "ACA", per_k>9) %>% 
#     arrange(desc(per_k)) %>% select(ngrams) -> test
#  ## decide what percentage of the corpus should contain the ngrams
#   at_least <- 5
# ## this filters only the ngrams whch are above a certain per k in a corpus
# corp_prop %>% filter(per_k > at_least)  -> corp_prop_min
# 
# # this shows how many filtered ngrams are in each corpus
# corp_prop_min %>% select(corpus,ngrams) %>% tally()
# length(unique(corp_prop_min$ngrams))
# ## find how many filtered ngrams are in the file_id
# ### join the new data to the freq_table
# left_join(corp_prop_min,freq_table) -> for_pivot
# for_pivot %>% ungroup() %>% group_by(file_id) %>% 
#   select(corpus,file_id,ngrams) %>% 
#   filter(corpus == "DEM") %>% tally() %>% print(n=200)

for_pivot <- freq_table %>%  select(corpus,file_id,ngrams,prop)

# in case there's a problem of dublicate rows, make sure everything is distinct in the df
for_pivot <-for_pivot %>% distinct()


pivot_table <- for_pivot %>% 
  pivot_wider(names_from = ngrams, # ngrams become the column names
              values_from = prop) # prop become the values of columns

percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(pivot_table, 1, percentmissing)

## change NAs to 0
pivot_table[is.na(pivot_table)] <- 0

## remove corpus header NOTE use ungroup() to avoid the 
#"Adding missing grouping variables: `corpus`" warning
pivot_table <- pivot_table %>% ungroup() %>% select(-corpus)

pivot_table <- freq_table %>% 
  pivot_wider(names_from = ngrams, # ngrams become the column names
              values_from = prop) # prop become the values of columns

pivot_test <- test %>% 
  pivot_wider(names_from = ngrams, # ngrams become the column names
              values_from = prop) # prop become the values of columns

# save result to csv file ####
# Data might be large, so use fwrite to save

fwrite(x = freq_table,
       file = paste0(output,today,"./data/output/full_freq_table_no_cut.csv"))

fwrite(x = pivot_table,
       file = paste0(output,today,"./data/output/full_pivot_table_cut@",cutoff,".csv"))


# create corpus variable and move it to the front takes a long time with full table!
# pivot_table$corpus <- gsubfn("([A-Z]{3}).*","\\1",pivot_table$file_id,perl=TRUE)

# pivot_table <- pivot_table %>%  select(corpus,everything())


## filtering ######
### by total > threshold: this method checks the overall number of ngrams in the corpora, 
# and first filters out those below a certain threshhold

# set threshold to how many texts should attest the ngrams
thresh <- 0.1

thresh_filt <- freq_table %>% group_by(ngrams) %>% 
  mutate(total =sum (prop)) %>% 
  filter(total > length(unique(freq_table$file_id))*thresh)%>% 

# to view the selected pos trigrams
thresh_filt %>% distinct(ngrams) %>% print(n=100)

# set threshold to how many texts should attest the ngrams
percent <- 0.1
thresh <- length(unique(freq_table$file_id))*percent


thresh_filt <- freq_table %>% group_by(ngrams) %>% 
  mutate(total =sum (prop)) %>% 
  filter(total > thresh) 

# to view the selected pos trigrams
thresh_filt %>% distinct(ngrams) %>% print(n=100)


# set threshold to how many texts should attest the ngrams
thresh <- 2

thresh_filt <- freq_table %>% group_by(ngrams) %>% 
  summarise(total =sum (prop)) %>%  
  filter(total > length(unique(freq_table$file_id))*thresh)

  select(file_id, ngrams, prop) 

# to view the selected pos trigrams
thresh_filt  %>% distinct(ngrams) %>% print(n=100)


# set threshold to how many texts should attest the ngrams
thresh_filt <- freq_table %>% group_by(ngrams) %>% 
  mutate(total =sum (prop)) %>% 
  filter(total > length(unique(freq_table$file_id))*thresh)%>% 
  select(file_id, ngrams, prop) 

# to view the selected pos trigrams
thresh_filt  %>% distinct(ngrams) %>% print(n=100)



## >>> go with the manageable 40 variables to start with ##
## >>> second look at 138 variables

# convert freq_table to a term freq matrix with pivot_wider ####
table_pivot <- thresh_filt %>% tidyr::pivot_wider(names_from = ngrams, # ngrams become the column names
                                                  values_from = prop) # prop become the values of columns


# # remove ngrams with punctuation  >> unnecesary if done earlier in the process
# table_pivot <- table_pivot[,-grep(pattern="PUNC",colnames(table_pivot))]

# count NAs in table 
sum(is.na(table_pivot)) 
# 12391 for 40 variables
# 76,660 for 138 variables
# 38,559 for 84 variables

## filter by na values - check more than 95% of rows have them  
## NOTE >>> this is doing the same thing as above when filtering on freq_table
#- ok with 40 variables
# 2 variables "lost" at 138 variables
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))<nrow(table_pivot)*.95)] ->NA_filt
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.95)] -> NA_kept

#   ## filter by na values - check more than 90% of rows have them 
#   #- ok with 40 variables
#   # 4 variables "lost" at 138 variables
#   table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))<nrow(table_pivot)*.9)] ->NA_filt
#   
# 
#   ## filter by na values - check more than 75% of rows have them 
#   #- ok with 40 variables
#   # 34 variables "lost" at 138 variables
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))<nrow(table_pivot)*.75)] ->NA_lost
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.3)] ->NA_kept
#   
## filter by na values - check more than 50% of rows have them
  #- ok with 40 variables
  # only 5 variables "retained" at 138 variables
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))<nrow(table_pivot)*.5)] ->NA_lost
  table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.5)] ->NA_kept
# 
# ## filter by na values - check more than 10% of rows have them
#   #- ok with 40 variables
#   # ok with 138 variables
# table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))<nrow(table_pivot)*.1)] ->NA_lost
# table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.1)] ->NA_lost
# 
# ## filter by na values - check more than 5% of rows have them - ok with 40 variables
# table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.05)] ->NA_filt

# ## filter by na values - check more than 1% of rows have them - ok with 40 variables
# table_pivot[,sapply(table_pivot, function(x) sum(!is.na(x))>nrow(table_pivot)*.001)] ->NA_filt

# DONT SKIP convert NAs to zero observations #####
table_pivot[is.na(table_pivot)] <- 0

##### check dispersion in corpora
## add corpus reference from file name
table_pivot$corpus <- gsubfn("([A-Z]{3}).*","\\1",table_pivot$file_id,perl=TRUE)

# put corpus name to front column
table_pivot %>% select(corpus,everything()) ->table_pivot

table_pivot <- pivot_table

# create duplicate object
binary_clone <- table_pivot

# binarise the dataframe >> the pos columns are now from third position to the end
# so wherever these are positive, change to 1 with ifelse expression
binary_clone[,3:ncol(binary_clone)] <- ifelse(binary_clone[,3:ncol(binary_clone)] > 0,1,0) 

## this shows how many files within each corpus contain at least one manifestation of pos trigram
corpus_disp<- binary_clone %>% 
  select(-file_id) %>% 
  group_by(corpus) %>% 
  summarise_each(funs(sum))


with(binary_clone, table(corpus, prog))


binary_clone %>% group_by(corpus) %>% 
  tally() -> corpus_count

corpus_prop <- corpus_disp[,2:ncol(corpus_disp)] / corpus_count$n
corpus_prop$corpus <- corpus_disp$corpus
corpus_prop %>% select(corpus,everything()) -> corpus_prop

# corpus_prop[4:5,] ->CMC_comp

# require(broom)
# 
# CMC_comp <- CMC_comp %>% mutate(corpus = factor(corpus))
# 
# glm(corpus ~ ., data = CMC_comp, family = binomial) -> model
# 
# tidy(model)
# model$rank
# # corpus_disp <- as_tibble(as.matrix(t(corpus_disp))) ## need as.matrix to transpose and keep names correctly

# save dispersion data  as files
write_delim(corpus_disp,paste0(output,"/pos_disp_threshold_",thresh,".txt"),delim = "\t",col_names = TRUE )
write_delim(corpus_prop,paste0(output,"/pos_prop_threshold_",thresh,".txt"),delim = "\t",col_names= TRUE )

## save pos trigram term frequency table without corpus column

term_freq_table <- table_pivot %>% select(-corpus)

write_csv(term_freq_table,paste0(output,"/3pos_",thresh,"_freq_table.csv"))


# dl <-  lapply(matched_files,function(x)  # list apply makes a list, running over the matched_files vector
#                           {y <- readLines(x)         # create y variable for the second function
#                           paste0(y,collapse = " ") # this flattens the text file, removing the new lines
#                           })
# names(dl) <- file_path_sans_ext(ba

