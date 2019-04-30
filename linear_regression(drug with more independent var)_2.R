data<-read.csv("drug2.csv")
summary(data)
str(data)
plot(data)
plot(data$sex)
plot(data$dose)
plot(data$response)

scatter.smooth(x=data$dose, y=data$response, main="dose ~ response")

boxplot(data$sex, main="sex", sub=paste("Outlier rows: ", boxplot.stats(data$sex)$out))
boxplot(data$dose, main="dose", sub=paste("Outlier rows: ", boxplot.stats(data$dose)$out))
boxplot(data$response, main="response", sub=paste("Outlier rows: ", boxplot.stats(data$response)$out))

plot(density(data$sex))
plot(density(data$dose))
plot(density(data$response))

cor(data$dose,data$response)
cor(data)

mod<-lm(response ~ dose+sex+dose*sex, data=data)
print(mod)
plot(mod)
summary(mod)
modsum<-summary(mod)

coeff<-modsum$coefficients
beta.estimate <- coeff["data$dose", "Estimate"]
std.error <- coeff["data$dose", "Std. Error"]
t_value <- beta.estimate/std.error
p_value <- 2*pt(-abs(t_value), df=nrow(data)-ncol(data))
f_statistic <- mod$fstatistic[1]
f <- summary(mod)$fstatistic
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

plot(density(mod$residuals))

scatter.smooth(x=mod$residuals, y=mod$fitted.values, main="error ~ predicted")
scatter.smooth(x=data$response, y=mod$fitted.values, main="predicted ~ response")
hist(mod$residuals)

library(Metrics)
rmse(data$response,mod$fitted.values)
