testCorpus<-Corpus(VectorSource(sts_gold_tweet[,3]))

testMat<-DocumentTermMatrix(testCorpus)

table(colSums(as.matrix(testMat)))

saved_model<-maxent(testMat[1:100,],as.factor(sts_gold_tweet$polarity)[1:100])

sum(sts_gold_tweet$polarity[101:150]-as.integer(predict(saved_model,testMat[101:150,])[,1])==0)

saved_model@weights

inspect(testMat[1,])

dim(testMat)

#bunch of trasforms
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

testCorpus1<-tm_map(testCorpus, toSpace, "\\.|/|\\||@")

testCorpus1<-tm_map(testCorpus1, removePunctuation)

testCorpus1<-tm_map(testCorpus1, removeWords, stopwords("english"))

testCorpus1<-tm_map(testCorpus1, removeWords, c("com","http"))

testCorpus1<-tm_map(testCorpus1, stripWhitespace)

testCorpus1<-tm_map(testCorpus1, stemDocument)

rm(toSpace)
#finish transforms

testMat<-DocumentTermMatrix(testCorpus1)

inspect(testMat[1,])

