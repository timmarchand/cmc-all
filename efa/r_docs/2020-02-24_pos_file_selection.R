
# this script loads the processed wmatrix tagged files, 
# and then generates pos text file versions for each text, counts the number of tokens
# and then selects the texts above a certain length and puts them in a new folder

#clear all
# rm(list=ls(all=TRUE))

# load libraries
library(tools)
library(tidyverse)
library(gsubfn)

# set the parameters

input_folder <- "/Users/Tim/Downloads/EFA_select/input"
setwd(input_folder) 
output <- "/Users/Tim/Downloads/EFA_select/output"
today <- paste0("/",Sys.Date(),"_")



mainFolder<-dir(input_folder) # get the subfolder names
###setup our directory 
subfolders<-paste0(input_folder,"/",mainFolder)
file_list <-  vector("list", length(mainFolder))
for(j in 1:length(subfolders)){

matched_files <- list.files(subfolders[j],pattern = ".txt",full.names = TRUE)



dl <-  lapply(matched_files,function(x)  # list apply makes a list, running over the matched_files vector
{y <- readLines(x)         # create y variable for the second function
paste0(y,collapse = " ") # this flattens the text file, removing the new lines
})
names(dl) <- file_path_sans_ext(basename(matched_files))


# create temp list
temp_list <- vector("list", length(dl))

for(i in 1:length(dl)){

temp_df <- NULL
  
text_id <- names(dl[i])
raw <-  as.character(unlist(dl[i]))
pos <-  gsubfn("([^_]+)_([^_]+)_([^_]+)(\\s)","\\2 ",x=raw,perl = TRUE)
tagcount <-unname(sapply(pos, function(x) length(unlist(strsplit(as.character(x), "\\W+")))))
punccount <- unname(sapply(pos, str_count, "PUNC"))
wc <- tagcount - punccount

temp_df <- as.data.frame(bind_cols(text_id = text_id,
                               tagged = raw,
                               pos = pos,
                             wordcount = wc), # this creates a named integer if unname not used above
                              stringsAsfactors= FALSE) # to avoid factor levels
# temp_df$wordcount <- wc # this way, integers remain unnamed if not unnmaed above

temp_list[[i]] <- temp_df

}

file_list[[j]] <- temp_list
}


file_df <- do.call(bind_rows,file_list)


# add corpus column
file_df$corpus <- gsubfn("([A-Z]{3}).*","\\1",file_df$text_id,perl=TRUE)
# move to front
file_df <- file_df %>% select(corpus,everything())

# save data
write_csv(file_df,paste0(output,today,"all_files_tag_pos.csv"))

# reload data  file_df ####
readLines(file.choose())
file_df <- read_delim(file.choose(),delim="\t")

# file_df <- read_csv(file.choose())
# 
# ## code for working out wordcounts:
# ##  NEWS: 10,000 > 98 files > 1000 wc
# file_df %>% select(corpus,text_id,wordcount) %>% 
#   filter(corpus == "NWS") %>% arrange(desc(wordcount)) %>% filter(wordcount > 1000) %>% tally()
# 
# #  NEWS: 10,000 > 98 files > 1000 wc
# file_df %>% select(corpus,text_id,pos,wordcount) %>%
#   filter(corpus == "NWS") %>% arrange(wordcount) %>% 
#   filter(wordcount < 10000 , wordcount > 1000) -> news_select

#  NEWS:  98 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>%
  filter(corpus == "NWS") %>% arrange(wordcount) %>% 
  filter(wordcount > 1000) -> news_select

##  ACA: 114 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "ACA") %>% arrange(wordcount) %>%  
  filter(wordcount > 1000) -> aca_select

##  FIC: 10000 > 189 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "FIC") %>% arrange(wordcount) %>%  
  filter(wordcount < 10000 , wordcount > 1000) -> fic_select

##  DEM: 265 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "DEM") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> dem_select

## HYC: 332 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "HYC") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> hyc_select

##  JUC: 103 files > 1000 wc
file_df %>% select(corpus,text_id,pos,wordcount) %>% 
  filter(corpus == "JUC") %>% arrange(wordcount) %>%  
  filter( wordcount > 1000) -> juc_select

pos_for_efa <- rbind(aca_select,dem_select,fic_select,news_select,hyc_select,juc_select)

## or in one line of code, set wordcount to above 1000
file_df <- file_df %>% filter(wordcount > 1000)

write_csv(file_df,path = paste0("/Users/Tim/Documents/R projects/Corpus text selection",today,"1000_pos_cut.csv"))

rm(list=c("aca_select","dem_select","fic_select","news_select","hyc_select","juc_select"))

pos_for_efa %>% group_by(corpus) %>% 
  summarise (count = n(), mean = mean(wordcount),sd = sd(wordcount),
             min_wc =min(wordcount),max_wc = max(wordcount),
            total = sum(wordcount)) -> pos_for_efa_table

## this is to save directly into the EFA project folder
output <- "/Users/Tim/Documents/R projects/EFA of POS/Input from file selection"

### save selection to file
write_delim(pos_for_efa,paste0(output,today,"pos_for_efa_selection.txt",delim = "\t"))
# and save the details to file
write_delim(pos_for_efa_table,paste0(output,today,"pos_for_efa_details.txt",delim ="\t"))

# reload data of pos_for_efa ####
pos_for_efa <- read_csv(file.choose())

# file_df <- read_csv(file.choose())


# # place list of searches into subsequent slot in the list of text files
# file_list[[i]] <- search_list
# 
# #### create a variable of the word count for each text
# #### requires all texts to be in a df, one text per row
# 
# method 1 -with fixed cutoff ####
# set cutoff point
cutoff <- 1000
# new df of texts above cutoff
texts_to_use <- file_df %>% filter(wordcount >= cutoff)
texts_omitted <- file_df %>% filter(wordcount < cutoff)

for (ii in 1:nrow(texts_to_use)){
  subd<-texts_to_use[ii,]
  er<-apply(data.frame(subd[,3]),1,paste0)
  writeLines(er, paste0(output,"/EFA/",texts_to_use[ii,1],".txt"))
}
cat(paste0(texts_to_use[ii,1],
           " will be the #",ii, 
           " used from the ", mainFolder[j]," corpus","\n",
           nrow(texts_omitted), " texts were omitted","\n\n"))
# for (jj in 1:nrow(texts_omitted)){
#   subd<-texts_omitted[jj,]
#   er<-apply(data.frame(subd[,3]),1,paste0)
#   writeLines(er, paste0(output,"/omit/",texts_omitted[jj,1],".txt"))
#   # if jj == nrow(texts_omitted)
#     # cat(paste0(jj," texts omitted from the",  mainFolder[j]," corpus","\n"))
# }
 # method 2 - by squeezing the wordcounts for a balanced # of texts ####

select_files <-pos_for_efa$text_id
# define the patterns for grepl
pattern <- paste(select_files, sep="", collapse="|")

# Now we can subset list with the following and change it into to adf
# NOTE use I to preserve the character strings
data.frame(I(file_df$pos[grepl(pattern,file_df$text_id)])) -> select_df
colnames(select_df) <- "pos"
select_df$text_id <- select_files

## alternative using %in%
file_df[which(file_df$text_id %in% select_files),] -> test
test <- test %>% select(pos)
test$text_id <- select_files

all.equal(test,select_df)

write_csv(select_df,path = paste0(output,today,"select_df_for_ptab.csv"))

## get the unselected files too
 unselect_files <- file_df$text_id[-grepl(pattern,file_df$text_id)]
 
 data.frame(I(file_df$pos[-grepl(pattern,file_df$text_id)])) -> unselect_df
 colnames(unselect_df) <- "pos"
 unselect_df$text_id <- unselect_files
 
 
 ## use all files #####
 select_df <- file_df %>% select(pos)
 select_df$text_id <- file_df$text_id
