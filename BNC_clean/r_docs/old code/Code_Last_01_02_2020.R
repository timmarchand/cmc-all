## install packages
install.packages(c("XML","rvest"))

## Libraryfor xml and reading data
library("XML")
library("methods")
library("rvest")
library(stringr)
library(readr)


### Choose the main folder that contains the subfolder of xml files
mainFolder<-"./data/BNC"
mainFoldr<-dir(mainFolder)
###setup our directory 
subfolders<-paste0(mainFolder,"/",mainFoldr)
## loop for each subfolder
for(kk in 1:length(subfolders)){
  ### get allxml file that are in the sub folder
  temp = list.files(path=subfolders[kk],pattern="*.xml") 
  #### read the xml file as text and not as xml
  myfiles = lapply(paste0(subfolders[kk],"/",temp), read_file)
  tep<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(temp))
  nae<-paste0(subfolders[kk],"/",tep)
  ### for each file
  for(k in 1:length(myfiles)){
### replace c tag with w tag so we can use punctuation too
        myfiles[[k]]<-gsub("<c","<w",myfiles[[k]])
    myfiles[[k]]<-gsub("</c","</w",myfiles[[k]])
    ### parse the new text file into xml
    xml <- xmlParse(myfiles[[k]])
    ### save the new xml file as rtemp.xml 
    saveXML(xml,paste0(subfolders[kk],"/rtemp.xml"))
  ### read this new xml file with new format (only w tags)
      myfiles[[k]]<-read_xml(paste0(subfolders[kk],"/rtemp.xml"))
  ## divide the file into sub content using levels value 
        Z<-myfiles[[k]] %>%
      xml_nodes("div[level='1']")
        
        ## for each content
    for (i in 1:length(Z)){
      ### get the text 
      F1<-Z[[i]] %>% xml_nodes("s") %>% html_text()
      nae1<-paste0(nae[k],i,".txt",collapse="")
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
      nae2<-paste0(nae[k],i,"_.txt",collapse="")
      writeLines(Q,nae2)
    }
  }
}
