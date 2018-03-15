GetReviewMetadata<-function(input.list){
  
  #get all review metadata from list
  review.metadata.list<- xpathApply(input.list, "//ul/li/div[not(@data-href)][not(@itemprop)]/meta")        
 
  #get the author,date and rating without tags
  metadata.without.tags<-lapply(review.metadata.list, function(x) xmlGetAttr(x,"content"))
  
  #for subsetting
  sub.seq<-seq(1,length(metadata.without.tags),3)
  
  #make final structure
  final.attr.struct<-cbind(metadata.without.tags[sub.seq],
                           metadata.without.tags[sub.seq+1],
                           metadata.without.tags[sub.seq+2])
  
  return(final.attr.struct)
}

#check function
GetReviewMetadata(list_seciter)

rm(GetReviewMetadata)

#review meta data
meta_review_list2<-xpathApply(list_seciter, "//ul/li/div[not(@data-href)][not(@itemprop)]/meta")

#get the author,date and rating
attr_list2<-lapply(meta_review_list2,function(x) xmlGetAttr(x,"content"))

#for subsetting
sub_seq<-seq(1,length(attr_list2),3)

#make final structure
final_attr_struct<-cbind(attr_list2[sub_seq],attr_list2[sub_seq+1],attr_list2[sub_seq+2])


