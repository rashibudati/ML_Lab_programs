data<-read.csv("knn.csv")
summary(data)
str(data)
table(data$diagnosis)
plot(data)
data$diagnosis<-ifelse(data$diagnosis=="B", 1, 0)
data$id<-NULL

m<-glm(diagnosis ~ ., data,family="binomial")
summary(m)

data$concavity_mean<-NULL

prop.table(table(data$diagnosis))
library(caret)
set.seed(1)
idx <- createDataPartition(data$diagnosis,p = 0.7,list = FALSE)
train <- data[idx,]
test <- data[-idx,]

prop.table(table(train$diagnosis))
prop.table(table(test$diagnosis))

library(class)
cl = train$diagnosis
datapred <- knn(train=train[,-1], test = test[,-1],cl, k=7)
print(datapred) 
library(gmodels)
t<-CrossTable(test[,1],datapred)
t1<-table(test[,1],datapred)
acc<-sum(diag(t1))/nrow(test)
print(acc)

acc1<-vector()
for(k in 1:30)
{
  datapred <- knn(train=train[,-1], test = test[,-1],cl, k)
  t1<-table(test[,1],datapred)
  acc1[k]<-sum(diag(t1))/nrow(test)
 
}
plot(acc1)

data$diagnosis<-as.factor(data$diagnosis)
trControl <- trainControl(method  = "cv",number= 10)
fit <- train(diagnosis ~ ., method= "knn",tuneGrid = expand.grid(k = 1:10),
             trControl  = trControl,metric = "Accuracy",data = data)
print(fit$results)

library(pROC)
plot(roc(test$diagnosis, as.numeric(datapred), direction="<"),
     col="yellow", lwd=3, main="curve")

for(i in 1:length(colnames(data))) {
  if(class(data[,i]) == "numeric" || class(data[,i]) == "integer") {
    data[,i] <- as.vector(scale(data[,i])) }
}

