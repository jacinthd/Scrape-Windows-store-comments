#split sentences into words
temp1<-strsplit(sts_gold_tweet$tweet," ")

#remove punctuation
temp2<-lapply(temp1,function(x) gsub("[[:punct:]]","",x))

#remove spaces
temp2<-lapply(temp2,function(x) gsub(" ","",x))

#make everything lowercase
temp2<-lapply(temp2,function(x) tolower(x))



#match the words to see if they're negative
temp_neg<-lapply(temp2,function(x) match(x,neg_words$V1))

#count the matched words in each sentence
temp_n<-lapply(temp_neg,function(x) !is.na(x))
neg_score<-sapply(temp_n,sum)



#match the words to see if they're positive
temp_pos<-lapply(temp2,function(x) match(x,pos_words$V1))

#count the matched words in each sentence
temp_p<-lapply(temp_pos,function(x) !is.na(x))
pos_score<-sapply(temp_p,sum)

#remove temp variables
rm(temp1,temp2,temp_n,temp_neg,temp_p,temp_pos)

#evaluate the algo's accuracy
score<-pos_score-neg_score
pol_by_list<-ifelse(score>0,"positive",ifelse(score==0,"neutral","negative"))

#convert it 0-4 form
pol_by_list2<-ifelse(pol_by_list=="positive",4,ifelse(pol_by_list=="negative",0,2))

tweet_sent_list_eval<-cbind(sts_gold_tweet,pol_by_list,pol_by_list2)

#find accuracy
sum(tweet_sent_list_eval$polarity==tweet_sent_list_eval$pol_by_list2)/nrow(tweet_sent_list_eval)

rm(pol_by_list,neg_score,pos_score,score,pol_by_list2)
