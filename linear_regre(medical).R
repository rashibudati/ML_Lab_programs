data<-read.csv("insurance1.csv")

summary(data)
str(data)

hist(data$expenses)

table(data$sex)
table(data$smoker)
table(data$region)

cor(data[c("age", "bmi", "children", "expenses")])

pairs(data[c("age", "bmi", "children", "expenses")])

model <- lm(expenses ~ ., data)
summary(model)

hist(model$residuals)

data$sex <-relevel(data$sex, ref = "male")
data$age2 <- data$age^2
model1 <- lm(expenses ~  age2 + children + bmi + sex + smoker + region, data=data)
summary(model1)                   
hist(model1$residuals)

model2 <- lm(expenses ~  age + age2 + children + bmi + sex + smoker + region, data=data)
summary(model2)                   
hist(model2$residuals)

data$bmi30 <- ifelse(data$bmi > 30, 1, 0)

model3<- lm(expenses ~  age + age2 + children + bmi30 + sex + smoker + region, data=data)
summary(model3)                   
hist(model3$residuals)

model4<- lm(expenses ~  age + age2 + children + bmi + bmi30 + sex + smoker + region, data=data)
summary(model4)                   
hist(model4$residuals)

model5<- lm(expenses ~  age + age2 + children + bmi + sex + bmi30*smoker + region, data=data)
summary(model5)                   
hist(model5$residuals)

