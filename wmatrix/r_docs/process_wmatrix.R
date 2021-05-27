
# libraries ---------------------------------------------------------------

pacman::p_load(rio, here,  tidyverse, fs, magrittr)


# data --------------------------------------------------------------------

input <- here("wmatrix","data")
txt_files <- fs::dir_ls(input, regexp = "\\.txt$", recurse = TRUE)


## zip alternative

# zipfile <- here("wmatrix","data", "wmatrix.zip")
# tmp <- tempfile()
# unzip(zipfile, exdir = tmp)
# 
# txt_files <- fs::dir_ls(tmp, regexp = "\\.txt$", recurse = TRUE)

# wrangle the data --------------------------------------------------------
## note the text files were copied  from wmatrix online site
## some basic editing was required to get the following code to work - especially
## filling missing lemma values whereever there was an 's after numbers

# create column names for the nested tables
new_cols <- c("id","count","pos","word","lemma")

# make the nested data table
wmatrix_dat <- 
# enframe the list of files, grab name of corpus
  txt_files %>% enframe() %>% transmute(corpus = basename(name) %>% path_ext_remove, path = value) %>% 
# import files by mapping path
  mutate(data =  map(.x = path, .f = ~ .x %>% import(quote = ""))) %>% 
# add corpus names to the data column lists
    mutate(data = set_names(data,corpus)) %>% 
# add the table column names
   mutate(data = map(data, set_names, new_cols)) %>% 
# find the textfile names (where id = 0 in the text file, filename  = lemma)
  mutate(data = map(data, ~.x %>%
# change the id to match the file name
  mutate(id = ifelse(id > 0,NA,lemma )) %>% 
# fill in the NA values with the filenames from above and remove filenames from lemma
 fill(id) %>% filter(id != lemma))) %>% 
# remove path file
  select(-path)
  
## save as rds object
  
saveRDS(wmatrix_dat,here("wmatrix","data","wmatrix_dat.rds") )

wmatrix_dat %>% pluck("data"
                      )

