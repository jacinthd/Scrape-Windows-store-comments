library(magrittr)

#get app_category
app_category<-      xpathApply(list_seciter,"//strong")[[1]]      %>%       xmlValue

#get app_name
app_name<-      xpathApply(list_seciter,"//h1[@itemprop]")[[1]]    %>%       xmlValue

#get price,publisher and software version
lapply(xpathApply(list_seciter,"//span[@itemprop]"),xmlAttrs)
           
#get number of reviews, file-size, last updated date, Supported languages and 1 extra shit
lapply(xpathApply(list_seciter,"//span[not(@itemprop)][not(@class)]"),xmlAttrs)

#get app description
app_desc<-xpathApply(list_seciter,"//div/div/pre[@itemprop='description']")
testdf2<-as.data.frame(cbind(testdf[,2],xmlValue(app_desc[[1]])))

#get all
app_attr_list<-xpathApply(list_seciter,"//strong|//h1[@itemprop]|//span[@itemprop]|//span[not(@itemprop)][not(@class)]")

testdf<-t(as.data.frame(sapply(app_attr_list,xmlValue)))


GetAppAttributes<-function(input.list){
  
  #get app category, app name, price, number of reviews, publisher, file-size, 
  #      last updated date, software version, Supported languages and microsoft-footer
  # the row.name and microsoft-footer will be gotten rid of later
  app.attr.list<-xpathApply(input.list,"//strong|//h1[@itemprop]|//span[@itemprop]|
                                        //span[not(@itemprop)][not(@class)]|
                                        //div/div/pre[@itemprop='description']")
  
  #convert this list to a dataframe
  app.attr.frame<-t(as.data.frame(sapply(app.attr.list,xmlValue)))

  return(app.attr.frame)
}

#check
testdf<-GetAppAttributes(list_seciter)

