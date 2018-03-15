#Issue 1: (edit: don't remember what is messed up)
#need to fix the messed up app description (probably due to ISO encoding used instead of UTF-8)

#Issue 2:
#Some weird warning in the reviews frame. Investigate what is it.

#Issue 3:
#Assign app id instead of app name in reviews frame to be safe

#Issue 4:
#maybe rewrite XML package stuff to rvest stuff? (Probably won't help)(seems like a bad idea)


#function to get each app's reviews and app data in 1 dataframe and 14 separate variables

ScrapeAllReviewData<-function(theurl){
  
  #function for getting URL as xml
  GetXmlFromUrl<-function(theurl){
    #list.first.iter<-htmlTreeParse(getURL( theurl, useragent="R"))
    #list.sec.iter<-list.first.iter$children$html
    #return(list.sec.iter)
    
    url.doc<-getURL( theurl, useragent="R")
    html.doc<-html(url.doc)
    return(html.doc)
  }
  
  #robust function to get app attributes
  RobustXpath<-function(input.list,input.function.no,input.path){
    found.value<-tryCatch(xpathApply(input.list,input.path),
                          error=function(cond){
                            return(NA)
                          })
    if(input.function.no){
      return(sapply(found.value,xmlValue))
    } else {
      return(sapply(found.value,function(x) xmlGetAttr(x,"content")))
    }
  }
  
  #function to get reviews and their metadata
  GetReviewFrame<-function(input.list){
    
    reviews.list<-xpathApply(input.list, "//ul/li/div[not(@data-href)][@itemprop]")
    
    review.metadata.list<- xpathApply(input.list, "//ul/li/div[not(@data-href)][not(@itemprop)]/meta")        
    
    #get the reviews only without html tags
    review.vec<-sapply(reviews.list,xmlValue)
    
    #get the author,date and rating without tags
    metadata.without.tags<-lapply(review.metadata.list,function(x) xmlGetAttr(x,"content"))
    
    #check if there are no reviews
    len.chk<-length(metadata.without.tags)
    
    if(len.chk==0){
      return(NULL)
    } else {
     
      #for subsetting
      sub.seq<-seq(1,len.chk,3)
      
      #make metadata structure
      review.meta.data.struct<-cbind(metadata.without.tags[sub.seq],
                                     metadata.without.tags[sub.seq+1],
                                     metadata.without.tags[sub.seq+2])
      
      #make review dataframe by joining reviews and its metadata
      review.frame<-as.data.frame(cbind(review.meta.data.struct,review.vec))
      
      return(review.frame)
    }
  }
    
  #function to get more url's from a document
  GetMoreUrl<-function(input.list){
    
    #for the case when there are no more links of reviews 
    if (theurl==0){
      return(0)
    } else {
      
      more_url<-tryCatch(xmlGetAttr(xpathApply(input.list,"//a[@id='moreReviews']")[[1]],"href"),
                         error=function(cond){
                           return(0)
                         })
      return(more_url)
    }
  }
  
  #more review frames
  ScrapeMoreReviews<-function(theurl){
    
    #for the case when there are no more reviews to pull
    if (theurl==0){
      return(NULL)
    } else {
     
      #this is similiar to the main program. DRY not followed
      xml.list<-GetXmlFromUrl(theurl)
      
      more.review.frame<-GetReviewFrame(xml.list)
      
      more.url<-GetMoreUrl(xml.list)
      
      #recursion!
      even.more.review.frame<-rbind(more.review.frame,ScrapeMoreReviews(more.url))
      
      return(even.more.review.frame)
    }
  }
  
  #MAIN LOGIC where all the above functions are called
  
  #get the site as html
  xml.list<-GetXmlFromUrl(theurl)
  
  
  #get app info
  #1 is for xmlValue and 0 for XmlGetAttr(x,"content")
  app.category<-RobustXpath(xml.list,1,"//strong[@itemprop='applicationCategory']")
  
  app.name<-RobustXpath(xml.list,1,"//h1[@itemprop='name']")
  
  app.price<-RobustXpath(xml.list,1,"//div[@id='offerRating']//span[@itemprop='price']")
  
  app.no.of.reviews<-RobustXpath(xml.list,1,"//div[@id='offerRating']//span[not(@itemprop)]")
  
  app.aggregate.rating<-RobustXpath(xml.list,0,"//div[@id='offerRating']//meta[@itemprop='ratingValue']")
  
  app.description<-RobustXpath(xml.list,1,"//div[@id='appDescription']")
  
  app.publisher<-RobustXpath(xml.list,1,"//div[@id='publisher']/span")
  
  app.size<-RobustXpath(xml.list,1,"//div[@id='packageSize']/span")
  
  app.last.update<-RobustXpath(xml.list,1,"//div[@id='releaseDate']/span")
  
  app.date.published<-RobustXpath(xml.list,0,"//div[@id='releaseDate']//meta[@itemprop='datePublished']")
  
  app.version<-RobustXpath(xml.list,1,"//div[@id='version']/span")
  
  app.languages<-RobustXpath(xml.list,1,"//div[@id='languages']//span") #All languages can be extracted!
  
  app.compatible.devices<-RobustXpath(xml.list,1,"//div[@id='softwareRequirements']/ul/li")
   
  app.required.permissions<-RobustXpath(xml.list,1,"//div[@id='hardwareRequirements']/ul/li")
  #finish getting app info
  
  
  #get reviews dataframe
  review.frame<- GetReviewFrame(xml.list)
  
  #more reviews' url
  more.url<-GetMoreUrl(xml.list)  
  
  #get all the review frames, even those in the "more" link
  review.frame.all<-rbind(review.frame,ScrapeMoreReviews(more.url))
  
  #convert reviews frame to pure dataframe instead of dataframe of lists
  review.frame.all<-as.data.frame((lapply(review.frame.all,unlist)),stringsAsFactors=FALSE)
 
  #add app name to reviews frame
  no.of.reviews<-nrow(review.frame.all)
  
  #try catch to check the case when there are no reviews
  review.frame.all$app_name<-tryCatch(rep(app.name,no.of.reviews),
                                      error=function(cond){
                                                 return(NULL)
                                                 })
  
  
           
   
  
  #get all 3 dataframes as a list
  all.review.data<-list(app.category, app.name, app.price, app.no.of.reviews, app.aggregate.rating, 
                        app.description, app.publisher, app.size, app.last.update, app.date.published, 
                        app.version, app.languages, app.compatible.devices, app.required.permissions, 
                        review.frame.all)
  
  print("1 app done")
  
  return(all.review.data)
}

rm(ijk)
test_list<-ScrapeAllReviewData("http://www.windowsphone.com/en-us/store/app/pandora/de2df279-485d-49bb-b53e-3f6a2a9401c1")

test.df<-(test_list[[15]])

rm(test_list,test.df)
