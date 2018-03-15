GetReviewBody<-function(input.list){
  
  #only review body
  reviews.list<-xpathApply(input.list, "//ul/li/div[not(@data-href)][@itemprop]")
  
  #get the reviews without html tags
  review.vec<-sapply(reviews.list,xmlValue)
  
  return(review.vec)
}

#to check the function
GetReviewBody(list_seciter)

rm(GetReviewBody)

#only review body
reviews_list<-xpathApply(list_seciter, "//ul/li/div[not(@data-href)][@itemprop]")

#get the reviews without html tags
review_vec<-sapply(reviews_list,xmlValue)

#final data structure
app_reviews_test<-as.data.frame(cbind(final_attr_struct,review_vec))

