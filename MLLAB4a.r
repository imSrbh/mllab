data<-read.csv("drug2.csv")

indexes <- sample(1:nrow(data), size=0.3*nrow(data))
test <- data[indexes,]
print(test)
train <- data[-indexes,]
print(train)

mod<-lm(response ~ dose+sex+dose*sex, data=train)
print(mod)
plot(mod)
summary(mod)
modsum<-summary(mod)

p1<-predict(mod,test)
summary(p1)
plot(density(p1))

rmse <- sqrt(sum((p1 - test$response)^2)/length(test$response))
c(RMSE = rmse, R2=summary(mod)$r.squared)
e <- vector(mode="numeric", length=0)
for(i in 1:960){
  e[i]=p1[i]-test$response[i]
}
hist(e)

scatter.smooth(x=e, y=p1, main="error ~ predicted")
scatter.smooth(x=p1, y=test$response, main="predicted ~ response")
plot(density(mod$residuals))
hist(mod$residuals)

library(Metrics)
rmse(train$response,mod$fitted.values)
rmse(test$response,p1)
