#1
data<-read.csv("bank.csv")
summary(data)
hist(data$age)
hist(data$salary)
table(data$repaid)

model<-glm(repaid ~ age,data,family="binomial")
summary(model)
hist(model$residuals)

model1<-glm(repaid ~ age+salary,data,family="binomial")
summary(model1)
hist(model1$residuals)


#2
data1<-read.csv("Bankloan.csv")
summary(data1)
plot(data1)

model3<-glm(y ~ .,data1,family="binomial")
summary(model3)
data1$default<-NULL
data1$education<-NULL
data1$loan<-NULL
data1$day_of_week<-NULL
data1$nr.employed<-NULL
data1$month<-NULL
data1$job<-NULL
data1$housing<-NULL
data1$marital<-NULL
data1$age<-NULL
data1$previous<-NULL
data1$euribor3m<-NULL
data1$pdays<-NULL

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for(i in 1:length(colnames(data1))) {
  if(class(data1[,i]) == "numeric" || class(data1[,i]) == "integer") {
    data1[,i] <- as.vector(normalize(data1[,i])) }
}

model4<-glm(y ~ .,data1,family='binomial')
summary(model4)



#3
data3<-read.csv("health.csv")
summary(data3)
str(data3)
data3$Suffered.Heart.storke<-as.factor(data3$Suffered.Heart.storke)
hist(data3$weight)
hist(data3$BP)
table(data3$Suffered.Heart.storke)
xtabs(~ Suffered.Heart.storke, data3)

m<-glm(Suffered.Heart.storke ~ weight + BP,data3,family="binomial")
summary(m)

m1<-glm(Suffered.Heart.storke ~ BP,data3,family="binomial")
summary(m1)

plot(data3$weight,data3$BP,col=data3$Suffered.Heart.storke)

bps<-data3$BP*data3$BP
weights<-data3$weight*data3$weight
weightbp<-data3$weight*data3$BP
data4<-cbind(data3,bps,weights,weightbp)
m2<-glm(Suffered.Heart.storke ~ BP + weight + weights + bps + weightbp, data4 ,family="binomial")
summary(m2)

m3<-glm(Suffered.Heart.storke~BP+weight+weights+bps, data4, family="binomial")
summary(m3)

p1<-predict(m3,data4,type="response")
head(p1)
head(data4)

pred1<-ifelse(p1>0.5,1,0)
tab1<-table(Predicted=pred1,Actual=data4$Suffered.Heart.storke)
print(tab1)
1-sum(diag(tab1))/sum(tab1)

with(m3, pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))

plot(data4$Suffered.Heart.storke~data4$BP+data4$weight+data4$weights+data4$bps)

predicteddata<-data.frame(m3$fitted.values,data4$Suffered.Heart.storke)
predicteddata<-predicteddata[order(m3$fitted.values,decreasing=FALSE),]
predicteddata$rank<-1:nrow(predicteddata)

library(ggplot2)
library(cowplot)

ggplot(data=predicteddata,aes(x=rank,y=m3$fitted.values))+
  geom_point(aes(color=data4$Suffered.Heart.storke),alpha=1,shape=4,stroke=2)+
  xlab("index")+ylab("predicted probability")




#4
#import dataset(text file)
summary(ICU)
str(ICU)
mod1<-glm(sta ~ .,ICU,family="binomial")
summary(mod1)
ICU$race<-NULL
ICU$inf<-NULL
ICU$crn<-NULL
ICU$cre<-NULL
ICU$hra<-NULL
ICU$po2<-NULL
ICU$bic<-NULL
ICU$ser<-NULL
ICU$fra<-NULL
ICU$cpr<-NULL
ICU$sex<-NULL
ICU$pre<-NULL
ICU$sys<-NULL
ICU$ph<-NULL
ICU$pco<-NULL
ICU$can<-NULL
mod2<-glm(sta ~ .,ICU,family="binomial")
summary(mod2)
library(ggplot2)
ggplot(data = ICU, aes(x = age, y = sta))  +
  geom_point(color = "transparent")  +
  geom_point(data = ICU, aes(x = age, y = mod2$fitted + 1, color = loc))

odds<-predict(mod2,ICU,type='response')
odds1<-predict(mod2,ICU)
plot(odds)
plot(odds1)
tapply(odds,ICU$sta,mean)

logLik(mod2)
lratio=-2*(logLik(mod2)-logLik(mod1))
pchisq(21.42, df=4, lower.tail=FALSE)

smp_size <- floor(0.70 * nrow(ICU))
set.seed(123)
train_ind <- sample(seq_len(nrow(ICU)), size = smp_size)

train <- ICU[train_ind, ]
test <- ICU[-train_ind, ]

mod3<-glm(sta~.,train,family = "binomial")
summary(mod3)
p<-predict(mod3, train, type="response")
p1<-predict(mod3,test,type="response")
tapply(p1,test$sta,mean)
library(caret)
confusionMatrix(table(p1,test$sta))
