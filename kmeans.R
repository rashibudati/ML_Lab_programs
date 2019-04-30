data<-read.csv("MLLAB1.CSV")
print(data)
summary(data)
str(data)
boxplot(data)
plot(data)
data$Region<-NULL
data$Channel<-NULL
df<-data
barplot(as.matrix(data))

set.seed(1)
df_cluster<-kmeans(df,5)
print(df_cluster)
summary(df_cluster)
str(df_cluster)
df_cluster$centers
table(df_cluster$cluster)

library(cluster)
library(fpc)
plotcluster(df,df_cluster$cluster)
clusplot(df,df_cluster$cluster,color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')

print(sse3<-sum(df_cluster$withinss))
print(sse4<-sum(df_cluster$betweenss))


library(animation)
kmeans.ani(df, 5)


sse1<-vector("numeric")
sse2<-vector("numeric")
for(i in 2:15){
sse1[i-1]=sum(kmeans(df,centers=i)$withinss)
sse2[i-1]<-sum(kmeans(df,centers=i)$betweenss)
}
sse1=as.data.frame(sse1)
sse2<-as.data.frame(sse2)
sse$k=seq.int(2,15)
plot(sse$k,sse1$sse,type="b")
plot(sse$k,sse2$sse,type='b')

rng<-2:20 
tries<-100
avg.totw.ss<-integer(length(rng)) 
for(v in rng){ 
  v.totw.ss<-integer(tries) 
  for(i in 1:tries){
    k.temp<-kmeans(df,centers=v) 
    v.totw.ss[i]<-k.temp$tot.withinss
  }
  avg.totw.ss[v-1]<-mean(v.totw.ss) 
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")






