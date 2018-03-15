library(RCurl)
library(XML)
library(rvest)
library(plyr)

list.all.apps<-htmlTreeParse(getURL( "http://www.windowsphone.com/en-us/store", useragent="R"))$children$html

#extract nodes with Urls
list.urls<-xpathApply(list.all.apps,"//div/div/div/a[@data-os='internalContent']")

#the nodes that need to be retained
seq.url.flags<-sapply(lapply(list.urls, function(x) grepl("Marketplace:Landing:[TN]",x)),sum)

#get those retained nodes
list.urls.needed<-ifelse(seq.url.flags,list.urls,NULL)[1:6]

#get url's from nodes
urls.needed<-lapply(list.urls.needed,function(x) xmlGetAttr(x,"href"))

rm(list.urls, seq.url.flags, list.urls.needed)

test.doc<-getURL( urls.needed[[1]], useragent="R")


pandora.list<-ScrapeAllReviewData("http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1")


all.app.urls<-lapply(urls.needed, GetAllApplinks)

all.apps.list<-unlist(all.app.urls)

#get all apps ka info
#takes around 15 minutes for 282 apps
#times out on slower internet connections
list.all.apps<-lapply(all.apps.list,ScrapeAllReviewData)






