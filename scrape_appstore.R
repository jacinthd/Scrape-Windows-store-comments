library(RCurl)
library(XML)

htmlTreeParse(getURL(
  "http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1"))

#this will work
htmlTreeParse(getURL(
  "http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1",
  .opts=list(useragent='Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3')))

#this is less verbose and less easy to understand as well
list_draft<-htmlTreeParse(getURL(
  "http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1",
  useragent="R"))

list_seciter<-list_draft$children$html

xpathApply(list_seciter, "//a[@href]")

xpathApply(list_seciter, "//ul/li/div")

xpathApply(list_seciter, "//ul/li/descendant::div")


