# clear all
rm(list=ls(all=TRUE))


# ## Libraries and folders set up -----------------------------------

## libraries
library("XML")
library("methods")
library("rvest")
library(stringr)
library(readr)
library(fs)
library(here)
library(purrr)

### Choose folders for input (containing xml files) and output
mainFolder<- here("BNC_clean","data","BNC")
outFolder <- here("BNC_clean","output","BNCtexts")
subfolders <- fs::dir_ls(mainFolder) %>% as.character
outfolders <- paste0(outFolder,"/", dir(mainFolder))
junk <- here("BNC_clean","junk")

files <- fs::dir_ls(recurse = TRUE, path = mainFolder, glob = "*xml")

purrr::map(files[1], read_file) %>% sub(pattern = "(.*)\\..*$", replacement = "\\1")


parse_file <- function(file) {
  file <- gsub("<c", "<w", file)
  file <- gsub("</c", "</w", file)
  xml <- xmlParse(file)
  saveXML(xml,paste0(junk,"/parse.xml"))
  xml <- read_xml(paste0(junk,"/parse.xml"))
  content <- xml %>% xml_nodes("div[level='1']")
  ### remove new xml file so that it is ignored in the following loop
 unlink(paste0(junk,"/parse.xml"))
  ## this divides the dem XML structure
  if (length(content) == 0) {
    content <- xml %>%
      xml_nodes("div")
  }
  return(content)
}

# function for creating file names removing path and extension
get_file_names <- function(files){
  name <- names(file) %>% basename(file) %>% path_ext_remove()
  return(name)
}

# function for saving text files
save_texts <- function(text,folder){
  paths <- file.path(folder, paste0( names(text), ".txt"))
  walk2(text,paths,write_lines)
}


# function putting it altogether
parse_folder <- function(in_folder,out_folder){
  files <- purrr::map(in_folder, read_file) %>% set_names(get_file_names) 
  texts <- files %>% parse_file %>% map(parse_text) %>% 
    set_names(nm =paste0(names(files),"-",sprintf("%03d",1:length(.)))) %>% 
    map(~paste(.x,collapse = "\n"))
  save_texts(texts,out_folder)
}

safe_parse <- safely(parse_folder)

parse_folder(files,outFolder)


walk(zxc,save_texts,outFolder)
zxc <- parse_folder(files[1])

asd <- purrr::map(files[1], read_file) %>% set_names(get_file_names) 
qwe <- asd %>% parse_file %>% map(parse_text) %>% 
  set_names(nm =paste0(names(asd),"-",sprintf("%03d",1:length(.))))

paths <- file.path(outFolder, paste0( names(zxc), ".txt"))
walk2(zxc,paths,write_lines)


file <- qwe
file <- gsub("<c", "<w", file)
file <- gsub("</c", "</w", file)
xml <- xmlParse(file)
saveXML(xml,paste0(junk,"/parse.xml"))
xml <- read_xml(paste0(junk,"/parse.xml"))
content <- xml %>% xml_nodes("div[level='1']")
## this divides the dem XML structure
if (length(content) == 0) {
  content <- xml %>%
    xml_nodes("div")
  content
}


parse_text <- function(content){
    ### get the text 
    text<-content %>% xml_nodes("s") %>% html_text()
    return(text)
}

safe_parse <- safely(parse_text)
map(content,parse_text)


content[[1]] %>% xml_nodes("s") %>% html_text()
## looping subfolders and grabbing text and tags----------------------------------------------

for(kk in 1:length(subfolders)){
  ### get all xml file that are in the sub folder
  temp = list.files(path=subfolders[kk],pattern="*.xml") 
  #### read the xml file as text and not as xml
  myfiles = lapply(paste0(subfolders[kk],"/",temp), read_file)
  tep<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(temp))
  nae<-paste0(outfolders[kk],"/",tep) 

  
  ### for each file
  for(k in 1:length(myfiles)){
    ### replace c tag with w tag so we can use punctuation too
    myfiles[[k]]<-gsub("<c","<w",myfiles[[k]])
    myfiles[[k]]<-gsub("</c","</w",myfiles[[k]])
    ### parse the new text file into xml
    xml <- xmlParse(myfiles[[k]])
    ### save the new xml file as rdait.xml 
    saveXML(xml,paste0(subfolders[kk],"/rdait.xml"))
    ### read this new xml file with new format (only w tags)
    myfiles[[k]]<-read_xml(paste0(subfolders[kk],"/rdait.xml"))
    ### remove new xml file so that it is ignored in the following loop
        unlink(paste0(subfolders[kk],"/rdait.xml"))
    ## divide the file into sub content using levels value 
        Z<-myfiles[[k]] %>%
      xml_nodes("div[level='1']")
    ## this divides the dem XML structure
        if(length(Z)==0){
      Z<-myfiles[[k]] %>%
        xml_nodes("div")
    }
        ## for each content
        for (i in 1:length(Z)){
          ### get the text 
          F1<-Z[[i]] %>% xml_nodes("s") %>% html_text()
      nae1<-paste0(nae[k],"-",sprintf("%03d",i),".txt",collapse="")
      # generate the normal text
      writeLines(paste(F1,sep=" "),nae1)
      ## seach for hw , c5 and value on the xml 
      t<-Z[[i]] %>% xml_nodes("w")
      a3<-t %>% html_text('table')
      a2<-t %>% xml_attr("c5")
      a1<- t %>% xml_attr("hw")
      ## replace NA with the same punc
      e<-which(is.na((a1)))
      a1[e]<-str_trim(a3)[e]
      ## combine the 3 attributes
      Q<-(paste0(str_trim(a3),"_",a2,"_",a1))
      Q<-paste0(Q,collapse=" ")
      Q<-gsub("_NA","",Q)
      ### save the tagged file
      nae2<-paste0(nae[k],"-",sprintf("%03d",i),"_tags.txt",collapse="")
      writeLines(Q,nae2)
    }
  }
}

# cleaning up files and folders -------------------------------------------


files <- list.files(path=mainFolder, pattern="*tags.txt", full.names=TRUE, recursive=TRUE)
lapply(files, function(x) {
  t <- readLines(x) # load file
  # apply function
  out <- function(t)
    # write to file
    write_lines(out, outFolder, sep="\n")
})
# copy the contents over to output directory
R.utils::copyDirectory(mainFolder, outFolder)


# remove the xml files from outfolder
junk <- dir(path=outFolder, pattern=".xml",recursive = TRUE) # match the xml 
file.remove(paste0(outFolder,"/",junk)) # remove the "junk"

# make another copy for the tagged files and both tagged and texts

R.utils::copyDirectory(outFolder, "./output/BNCtagged")
R.utils::copyDirectory(outFolder, "./output/BNCall")

## return the data file back to original state
for(kk in 1:length(subfolders)){
  ### grab all but the xml file that are in the sub folder
  # use grep to enable invert argument
tat <- grep(list.files(path=subfolders[kk]), pattern='.xml', invert=TRUE, value=TRUE)
fs::dir_delete(paste0(subfolders[kk],"/",tat))

tagFolder <- dir("./output/BNCtagged")
tagfolders <- paste0("./output/BNCtagged/",tagFolder)

for(kk in 1:length(tagfolders)){
  ### grab all but the xml file that are in the sub folder
  # use grep to enable regex pattern search
  tat <- grep(list.files(path=tagfolders[kk]), pattern='^texts$',  value=TRUE)
  fs::dir_delete(paste0(tagfolders[kk],"/",tat))
}

textFolder <- dir("./output/BNCtexts")
textfolders <- paste0("./output/BNCtexts/",textFolder)

for(kk in 1:length(textfolders)){
  ### grab all but the xml file that are in the sub folder
  # use grep to enable regex pattern search
  tat <- grep(list.files(path=textfolders[kk]), pattern='^tagged$',  value=TRUE)
  fs::dir_delete(paste0(textfolders[kk],"/",tat))
}
