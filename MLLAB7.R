data<-read.csv("adult.csv")

summary(data)
str(data)

data$NATIVECOUNTRY<-NULL
data$MARITALSTATUS<-NULL
data$RELATIONSHIP<-NULL
data$RACE<-NULL
data$WORKCLASS<-NULL
data$EDUCATION<-NULL
data$OCCUPATION<-NULL
data$SEX<-NULL
summary(data)
str(data)


table(data$ABOVE50K)
ip1<-data[which(data$ABOVE50K==1),]
ip0<-data[which(data$ABOVE50K==0),]

set.seed(1)
ip1tr<-sample(1:nrow(ip1),0.7*nrow(ip1))
ip0tr<-sample(1:nrow(ip0),0.7*nrow(ip0))

tr1<-ip1[ip1tr,]
tr0<-ip0[ip0tr,]
data1<-rbind(tr1,tr0)

ts1<-ip1[-ip1tr,]
ts0<-ip0[-ip0tr,]
data2<-rbind(ts1,ts0)

model<-glm(ABOVE50K ~ AGE + CAPITALGAIN + EDUCATIONNUM + FNLWGT + CAPITALLOSS + HOURSPERWEEK, data=data1, family=binomial(link="logit"))
summary(model)

logistrain<-predict(model,type='response')
plot(logistrain)
tapply(logistrain,data1$ABOVE50K,mean)
trn_pred1 <- ifelse(predict(model, type = "response") > 0.5, "1", "0")
trn_tab1 <- table(predicted = trn_pred, actual = data1$ABOVE50K)
print(trn_tab)

predicted <- predict(model, data2, type="response")
plot(predicted)
tapply(predicted,data2$ABOVE50K,mean)
trn_pred2 <- ifelse(predicted > 0.5, "1", "0")
trn_tab2 <- table(predicted = trn_pred, actual = data2$ABOVE50K)
print(trn_tab2)

library(caret)
confusionMatrix(trn_tab2, positive = "1")
