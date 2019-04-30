data<-read.csv("iriswc.csv")
str(data)
summary(data)
data$class<-as.factor(as.numeric(data$class))
hist(data$Slen)
hist(data$Swid)
hist(data$Plen)
hist(data$Pwid)
table(data$class)

plot(data$Slen,data$Swid,col=data$class)
plot(data$Plen,data$Pwid,col=data$class)
library(nnet)
m<-multinom(class ~ Slen + Swid + Plen + Pwid, data)
summary(m)

p1<-predict(m,data,type="prob")
head(p1)
head(data)

tab<-table(predict(m),data$class)
print(tab)
p2<-predict(m,data,type="class")
1-sum(diag(tab))/sum(tab)

plot(data$class~data$Slen+data$Swid+data$Plen+data$Pwid)

predicteddata<-data.frame(m$fitted.values,data$class)
predicteddata<-predicteddata[order(m$fitted.values,decreasing=FALSE),]
predicteddata$rank<-1:nrow(predicteddata)


library(ROCR)
plot(multiclass.roc(data$class, as.numeric(p2)),direction="<<",
     col="yellow",lwd=3,main="curve")

library(pROC)
plot(roc(data$class, as.numeric(p2), direction="<"),
     col="yellow", lwd=3, main="curve")

