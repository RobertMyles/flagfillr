library(rvest)

# urls
# base url: https://en.wikipedia.org/wiki/
flags <- c("Argentine", "Armenian", "Australian", "Austrian", "Bangladeshi", "Belgian",
           "Canadian", "Chilean", "Chinese", "Colombian", "Danish", "Dutch", "Egyptian",
           "French", "German", "Greek", "Hungarian", "Icelandic", "Indian", "Italian", 
           "Japanese", "Mexican", "Pakistani", "Portuguese", "Russian", "Spanish", 
           "South_African", "Korean")
url_list <- vector("list", length(flags))
base_url <- "https://en.wikipedia.org/wiki/List_of_"
for(i in 1:length(url_list)){
  url_list[[i]] <- paste0(base_url, flags[[i]], "_flags")
}
library(rvest)
imginfo <- read_html(url_list[[1]]) %>% 
  html_nodes("#mw-content-text > div > table:nth-child(13) > tbody > tr:nth-child(1) > td:nth-child(1) > a") %>% 
  html_attr("href")
img.url<- imginfo[1] %>% html_attr("href")
img.url<-paste0("https:",img.url)
if(is.na(savedest)){
  savefilename<-paste0(ttl,".jpg")
}else{savefilename<-paste0(savedest,ttl,".jpg")}

if(res!=220){img.url<-gsub(220,res,img.url)}  

download.file(img.url,savefilename)
return(paste0("orig.file: ",basename(img.url)))#tell user original filename (or error)
