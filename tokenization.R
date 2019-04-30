data<-read.csv("sms_spam.csv")
summary(data)
str(data)
unique(data$type)
unique(data$text)

library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(data$text))
summary(corpus)
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, content_transformer(tolower))
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, removeNumbers)
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, removePunctuation)
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, removeWords, stopwords())
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, stemDocument)
for(i in 1:5){
  print(corpus[[i]]$content)
}
corpus = tm_map(corpus, stripWhitespace)
for(i in 1:5){
  print(corpus[[i]]$content)
}

library(wordcloud)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm,0.999)
dataset = as.matrix(dtm)
v = sort(colSums(dataset),decreasing=TRUE)
myNames = names(v)
d = data.frame(word=myNames,freq=v)
wordcloud(d$word, colors=c(3,4),random.color=FALSE, d$freq, min.freq=270)

df<-as.data.frame(dataset)

data$type<-ifelse(data$type=='spam',1,0)
df<-cbind(df,data$type)
summary(df)
str(df)



