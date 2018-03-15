#get all App links from the top free apps, paid apps etc.

GetAllApplinks<-function(theurl){
  
  #get the webpage
  url.doc<-getURL( theurl, useragent="R")
  
  #get all links from the page
  app.urls.vec<- html(url.doc) %>% html_nodes("#main .title5") %>% html_attr("href")
  
  return(app.urls.vec)
}
