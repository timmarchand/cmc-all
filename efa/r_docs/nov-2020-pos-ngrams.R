

# this script loads the processed wmatrix tagged files, 
# and then generates pos text file versions for each text, counts the number of tokens
# and then selects the texts above a certain length and puts them in a new folder

#clear all
rm(list=ls(all=TRUE))

# load libraries
library(tools)
library(tidyverse)
library(gsubfn)
library(here)
library(fs)
library(parameters)
library(psych)
library(ngram)
# set the parameters

input<- here("data","corpora")
output <- here("data","output")
today <- paste0("/",Sys.Date(),"_")

txt_files <- fs::dir_ls(input, regexp = "\\.txt$", recurse = TRUE)
txt_files

# get the file names as a vector
file_names <- list.files(input, recursive = T)  %>% 
  # sub(pattern = "(.*)\\..*$", replacement = "\\1") %>% 
  sub(pattern = ".../", replacement = "") %>% 
  sub(pattern = ".txt", replacement = "")

## create list of txt files
## set names to file names
txt_list <- txt_files %>% 
  map(read_file) %>% set_names(file_names)
names(xml_list)

# function to read in txt files
read_txt <- function(x)  # list apply makes a list, running over the matched_files vector
{raw <- readLines(x)         # create y variable for the second function
paste0(raw,collapse = " ") # flatten, removing line breaks
tibble(raw) # turn to df for later wrangling
}



dl <-  map(txt_files,read_txt) %>% # map read_text function on txt_files
  set_names(file_names) #  create a named list


# function for getting the tag into a df  
  get_tag_info <- function(raw){
    raw %>% 
    mutate(pos = gsubfn("([^_]+)_([^_]+)_([^_]+)(\\s)","\\2 ",x=.,perl = TRUE), # keep the tags
           tagcount = unname(sapply(pos, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))),
           punccount = unname(sapply(pos, str_count, "PUNC")),
           wc = tagcount - punccount)
  }

  # apply to list
dl2 <- map(dl,get_tag_info)

# add corpus and text_id details based on list names
dl3 <- imap(dl2, ~mutate(.x, 
                         corpus =  gsubfn("([A-Z]{3}).*","\\1",.y,perl=TRUE),
                         text_id = gsubfn("([A-Z]{3}_)(.*)","\\2",.y,perl=TRUE),
                         mode = if_else(corpus %in% c("ACA","FIC","NNW"),true = "WTN",
                                            false = if_else(corpus == "DEM", true = "SPN", 
                                                            false = "CMC"))))

## put text identifiers at the front of each df
dl4 <- map(dl3, ~select(.x, text_id,mode, corpus, everything()))


## create cleanPOS function 
cleanPOS <- function(x){
  x <- gsubfn("-----PUNC-----","<<END>>",x)
  x <- gsubfn("[^_ ]+_([^_]+)_[^_ ]+","\\1",x, perl = TRUE)
  x <- gsubfn("@","",x)
  x <- gsubfn("%","",x)}

## add cleaned pos column
dl5 <- map(dl4,~mutate(.x, clean_pos = cleanPOS(pos)))

## save as dataframe, binding rows from list
all_pos_df <-  do.call(bind_rows,dl5)


make_pos_table <- function(df,cut = 0){
  df %>% 
    filter(wc > cut) %>% 
  group_by(corpus) %>% 
    summarise (count = n(), 
               mean_wc = mean(wc),
               sd = sd(wc),
               min_wc =min(wc),
               max_wc = max(wc),
               total = sum(wc))
}

all_pos_table <- make_pos_table(all_pos_df)
all_pos_table

cut800_pos_table <- make_pos_table(all_pos_df, 800)
cut800_pos_table

cut600_pos_table <- make_pos_table(all_pos_df, 600)
cut600_pos_table


### ngram stuff ----




## function to create a nested df of ngram table

ng_table <- function(x){
  x %>% 
    ngram::ngram(3) %>% 
    get.phrasetable() %>% tibble
}


## filtering function removing ngrams which overlap punctuation boundaries
filter_punc <- function(df){
  df %>% filter(!grepl("PUNC",ngrams),!grepl("[^A-Z0-9 ]",ngrams))
}

## filtering by word count
filter_wc <- function(df,x){
  df %>% filter(wc > x)
}


map(dl5,~filter_wc(.x,800))

## get ngram tables for each text using clean_pos
pos_for_800 <- dl5 %>% map(~filter_wc(.x,800))  %>% 
             do.call(bind_rows,.)


## get ngram tables for each text using clean_pos
pos_for_600 <- dl5 %>% map(~filter_wc(.x,600))  %>% 
  do.call(bind_rows,.)

             
ng_list_800 <- 
  map(pos_for_800$clean_pos,ng_table) %>% 
  set_names(paste0(pos_for_800$corpus,"_",pos_for_800$text_id)) %>%  # retain corpus and text_id info
  map(filter_punc) # remove ngrams overlapping punctuation boundaries
 # filter word count


# add text_id and corpus columns
ng_list_800 <- ng_list_800 %>% imap(~mutate(.x, 
        corpus =  gsubfn("([A-Z]{3}).*","\\1",.y,perl=TRUE),
        text_id = gsubfn("([A-Z]{3}_)(.*)","\\2",.y,perl=TRUE)))

# change list to a df
ng_df_600 <- do.call(bind_rows,ng_list_600)

ng_list_600 <- 
  map(pos_for_600$clean_pos,ng_table) %>% 
  set_names(paste0(pos_for_600$corpus,"_",pos_for_600$text_id)) %>%  # retain corpus and text_id info
  map(filter_punc) # remove ngrams overlapping punctuation boundaries
# filter word count


# add text_id and corpus columns
ng_list_600 <- ng_list_600 %>% imap(~mutate(.x, 
                                            corpus =  gsubfn("([A-Z]{3}).*","\\1",.y,perl=TRUE),
                                            text_id = gsubfn("([A-Z]{3}_)(.*)","\\2",.y,perl=TRUE)))

# change list to a df
ng_df_600 <- do.call(bind_rows,ng_list_600)

# created df with nested columns
ng_df %>% group_by(text_id) %>% nest(ng_table = 1:3) -> nested_ng_df

## setting a filter of ngram > 10% in the corpus
# this obtains the frequency totals of each ngram in each corpus
ng_df_800 %>% group_by(corpus,ngrams) %>%
  summarise(total = sum(freq))  -> freq_sums
# this tallies the total number of rows for each corpus
ng_df_800 %>% group_by(corpus) %>% tally() -> corp_sum
# this creates a table of the proportion of each ngram within each corpus per thousand
left_join(freq_sums,corp_sum) %>%
  mutate(corp_prop = total / n, per_k = corp_prop*1000) %>% 
  filter(per_k >1) -> filtered_freq

### set threshold to how many texts should attest the ngrams
percent <- 0.1  # 10%
thresh <- length(unique(ng_df_800$text_id))*percent


thresh_filt <- ng_df_800 %>% group_by(ngrams) %>% 
  mutate(total =sum (prop)) %>% 
  filter(total > thresh) 

thresh_filt

for_pivot <- inner_join(ng_df,filtered_freq)

for_pivot %>% select(ngrams) %>% unique() %>% nrow
  
)

## the pivot and EFA prep----



## select only identifying variables before doing the pivot
pivot_table <- for_pivot %>% select(corpus,text_id,ngrams,prop) %>% 
  pivot_wider(names_from = ngrams, # ngrams become the column names
              values_from = prop) %>% # prop become the values of columns
  janitor::clean_names() # clean the variable names
## calculate the coverage of the ngrams by percent missing
percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(pivot_table, 1, percentmissing)
missing %>% range
missing %>% tibble %>% ggplot + aes(.) + geom_histogram() 

master <- pivot_table %>% select(-corpus,-text_id)
rownames(master) <- pivot_table$text_id

##missing
percentmissing <- function (x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)

master[is.na(master)] <- 0
apply(master, 2 , function(x) sum(x)) -> sum_cols
master <- master[sum_cols>0]


master[is.na(master)] <- 0
noout <- master

##exclude the ngram missing too much data
master10 <- subset(master, missing <= 90)

##make sure the columns aren't missing too much
apply(replacengram, 2, percentmissing)

cutoff <- qchisq(1-.001, ncol(master))

mahal <- mahalanobis(master,
                     colMeans(master),
                     cov(master))
cutoff ##cutoff score
ncol(master) ##df
summary(mahal < cutoff)

##exclude outliers
master_noout <- subset(master, mahal < cutoff)

##additivity
correl <- cor(master_noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random <- rchisq(nrow(master_noout), 4)
fake <- lm(random~., data = master_noout)
standardized <- rstudent(fake)
fitted <- scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

# test for KMO and Bartlett
check_factorstructure(master)

# find optimal number of factors - very slow!
nf <- parameters::n_factors(master,type = "oblique",package = c( "psych"))
nf

##how many factors?
nofactors <- fa.parallel(master_noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion


# write_csv(file_df,paste0(output,today,"all_files_tag_pos.csv"))
# 
# # reload data  file_df ####
# readLines(file.choose())
# file_df <- read_delim(file.choose(),delim="\t")
# 
# # file_df <- read_csv(file.choose())
# # 
# # ## code for working out wordcounts:
# # ##  NEWS: 10,000 > 98 files > 1000 wc
# # file_df %>% select(corpus,text_id,wordcount) %>% 
# #   filter(corpus == "NWS") %>% arrange(desc(wordcount)) %>% filter(wordcount > 1000) %>% tally()
# # 
# # #  NEWS: 10,000 > 98 files > 1000 wc
# # file_df %>% select(corpus,text_id,pos,wordcount) %>%
# #   filter(corpus == "NWS") %>% arrange(wordcount) %>% 
# #   filter(wordcount < 10000 , wordcount > 1000) -> news_select
# 
# #  NEWS:  98 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>%
#   filter(corpus == "NWS") %>% arrange(wordcount) %>% 
#   filter(wordcount > 1000) -> news_select
# 
# ##  ACA: 114 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>% 
#   filter(corpus == "ACA") %>% arrange(wordcount) %>%  
#   filter(wordcount > 1000) -> aca_select
# 
# ##  FIC: 10000 > 189 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>% 
#   filter(corpus == "FIC") %>% arrange(wordcount) %>%  
#   filter(wordcount < 10000 , wordcount > 1000) -> fic_select
# 
# ##  DEM: 265 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>% 
#   filter(corpus == "DEM") %>% arrange(wordcount) %>%  
#   filter( wordcount > 1000) -> dem_select
# 
# ## HYC: 332 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>% 
#   filter(corpus == "HYC") %>% arrange(wordcount) %>%  
#   filter( wordcount > 1000) -> hyc_select
# 
# ##  JUC: 103 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>% 
#   filter(corpus == "JUC") %>% arrange(wordcount) %>%  
#   filter( wordcount > 1000) -> juc_select
# 
# pos_for_efa <- rbind(aca_select,dem_select,fic_select,news_select,hyc_select,juc_select)
# 
# ## or in one line of code, set wordcount to above 1000
# file_df <- file_df %>% filter(wordcount > 1000)
# 
# write_csv(file_df,path = paste0("/Users/Tim/Documents/R projects/Corpus text selection",today,"1000_pos_cut.csv"))
# 
# rm(list=c("aca_select","dem_select","fic_select","news_select","hyc_select","juc_select"))
# 
# pos_for_efa %>% group_by(corpus) %>% 
#   summarise (count = n(), mean = mean(wordcount),sd = sd(wordcount),
#              min_wc =min(wordcount),max_wc = max(wordcount),
#              total = sum(wordcount)) -> pos_for_efa_table
# 
# ## this is to save directly into the EFA project folder
# output <- "/Users/Tim/Documents/R projects/EFA of POS/Input from file selection"
# 
# ### save selection to file
# write_delim(pos_for_efa,paste0(output,today,"pos_for_efa_selection.txt",delim = "\t"))
# # and save the details to file
# write_delim(pos_for_efa_table,paste0(output,today,"pos_for_efa_details.txt",delim ="\t"))
# 
# # reload data of pos_for_efa ####
# pos_for_efa <- read_csv(file.choose())
# 
# # file_df <- read_csv(file.choose())
# 
# 
# # # place list of searches into subsequent slot in the list of text files
# # file_list[[i]] <- search_list
# # 
# # #### create a variable of the word count for each text
# # #### requires all texts to be in a df, one text per row
# # 
# # method 1 -with fixed cutoff ####
# # set cutoff point
# cutoff <- 1000
# # new df of texts above cutoff
# texts_to_use <- file_df %>% filter(wordcount >= cutoff)
# texts_omitted <- file_df %>% filter(wordcount < cutoff)
# 
# for (ii in 1:nrow(texts_to_use)){
#   subd<-texts_to_use[ii,]
#   er<-apply(data.frame(subd[,3]),1,paste0)
#   writeLines(er, paste0(output,"/EFA/",texts_to_use[ii,1],".txt"))
# }
# cat(paste0(texts_to_use[ii,1],
#            " will be the #",ii, 
#            " used from the ", mainFolder[j]," corpus","\n",
#            nrow(texts_omitted), " texts were omitted","\n\n"))
# # for (jj in 1:nrow(texts_omitted)){
# #   subd<-texts_omitted[jj,]
# #   er<-apply(data.frame(subd[,3]),1,paste0)
# #   writeLines(er, paste0(output,"/omit/",texts_omitted[jj,1],".txt"))
# #   # if jj == nrow(texts_omitted)
# #     # cat(paste0(jj," texts omitted from the",  mainFolder[j]," corpus","\n"))
# # }
# # method 2 - by squeezing the wordcounts for a balanced # of texts ####
# 
# select_files <-pos_for_efa$text_id
# # define the patterns for grepl
# pattern <- paste(select_files, sep="", collapse="|")
# 
# # Now we can subset list with the following and change it into to adf
# # NOTE use I to preserve the character strings
# data.frame(I(file_df$pos[grepl(pattern,file_df$text_id)])) -> select_df
# colnames(select_df) <- "pos"
# select_df$text_id <- select_files
# 
# ## alternative using %in%
# file_df[which(file_df$text_id %in% select_files),] -> test
# test <- test %>% select(pos)
# test$text_id <- select_files
# 
# all.equal(test,select_df)
# 
# write_csv(select_df,path = paste0(output,today,"select_df_for_ptab.csv"))
# 
# ## get the unselected files too
# unselect_files <- file_df$text_id[-grepl(pattern,file_df$text_id)]
# 
# data.frame(I(file_df$pos[-grepl(pattern,file_df$text_id)])) -> unselect_df
# colnames(unselect_df) <- "pos"
# unselect_df$text_id <- unselect_files
# 
# 
# ## use all files #####
# select_df <- file_df %>% select(pos)
# select_df$text_id <- file_df$text_id
# 
# 
# ##import the file from POS project
# master <- read_csv(here("data","2020-02-26_full_freq_table_no_cut.csv")
# ## remove duplicates
# master <- master %>% distinct()
# 
# master$corpus <- gsub("([A-Z]{3}).*","\\1",master$file_id,perl=TRUE)
