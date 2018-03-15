#get and clean review data
all.review.list<-lapply(list.all.apps,"[[",15)

all.reviews.df<-rbind.fill(all.review.list)

colnames(all.reviews.df)<-c("User.name","post.date","star.rating","review","App.name")


#create app data frame(more important stuff in this frame)
app.attr.frame<-data.frame(matrix(ncol = 10, nrow = 282))

#convert list of app attributes to dataframe
for(i in c(1:5,7:11)){
  if (i<7){
    app.attr.frame[i]<-as.character(lapply(list.all.apps,"[[",i))
  } else {
    app.attr.frame[i-1]<-as.character(lapply(list.all.apps,"[[",i))
  }
}


#replace some strings with NA
app.attr.frame[app.attr.frame=="list()"]<-NA

app.attr.frame[app.attr.frame=="\r\n            "]<-NA

app.attr.frame[app.attr.frame==""]<-NA

#give proper colname
colnames(app.attr.frame)<-c("App.category","Name","Price","#reviews","Avg.rating","Publisher","Size","last.update","date.published","Current.version")

#rearrange cols
app.attr.frame<-app.attr.frame[c(2,1,3:10)]

#format dates
app.attr.frame$last.update<-as.Date(app.attr.frame$last.update,"%m/%d/%Y")



#create app data frame(less important stuff in this frame)
app.attr.frame.2<-data.frame(matrix(ncol = 5, nrow = 282))

for(i in c(2,6)){
  app.attr.frame.2[i-1]<-as.character(lapply(list.all.apps,"[[",i))
}

#make list of words into one to put in data frame
for(i in 12:14){
  app.attr.frame.2[i-10]<-as.character(lapply(lapply(list.all.apps,"[[",i),function(x) paste(x,collapse=", ")))
}

#replace some strings with NA
app.attr.frame.2[app.attr.frame.2==""]<-NA

app.attr.frame.2[app.attr.frame.2=="list()"]<-NA

colnames(app.attr.frame.2)<-c("App.name","supported.languages","supported.os","installation.permissions","Description")



#write files
write.csv(all.reviews.df,"D:/sentiment_analysis/Reviews.csv")

write.csv(app.attr.frame,"D:/sentiment_analysis/App_data1.csv")

write.csv(app.attr.frame.2,"D:/sentiment_analysis/App_data2.csv")

