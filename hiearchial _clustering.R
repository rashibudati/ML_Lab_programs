data<-read.csv("MLLAB1.CSV")

summary(data)
str(data)
boxplot(data)

data$Region<-NULL
data$Channel<-NULL
df<-data
barplot(as.matrix(data))

distmat<-dist(df)
hclust_com<-hclust(distmat,method='average')
plot(hclust_com)

cut_com<-cutree(hclust_com,k=3)
print(cut_com)
plot(cut_com)
plot(hclust_com)
rect.hclust(hclust_com , k = 3)


library(dplyr)
df_cl <- mutate(df, cluster = cut_com)
count(df_cl,cluster)
library(ggplot2)
ggplot(df, aes(Fresh,Milk)) + geom_point(alpha = 0.4, size = .5) + geom_point(col = cut_com) + 
  scale_color_manual(values = c('black', 'red', 'green','yellow','blue')) 

table(df_cl$cluster)
library(cluster)
library(fpc)
plotcluster(df,df_cl$cluster)

newdata<-df_cl[order(df_cl$cluster),]

data1<-subset(newdata,newdata$cluster==1)
data2<-subset(newdata,newdata$cluster==2)
data3<-subset(newdata,newdata$cluster==3)
k1<-kmeans(data1,1)
k2<-kmeans(data2,1)
k3<-kmeans(data3,1)
ttwsse<-k1$withinss+k2$withinss+k3$withinss
sum1<-0.0
sum2<-0.0
sum3<-0.0
for(i in 1:7){
  sum1<-sum1+(k1$centers[i]-k2$centers[i])^2
  sum2<-sum2+(k2$centers[i]-k3$centers[i])^2
  sum3<-sum3+(k3$centers[i]-k1$centers[i])^2
}
d1<-sqrt(sum1)
d2<-sqrt(sum2)
d3<-sqrt(sum3)
ttbsse<-d1+d2+d3
print(ttwsse)
print(ttbsse)
print(ttwsse+ttbsse)







