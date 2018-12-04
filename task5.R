#Linear Regression  Model

library(MASS)
attach(UScrime)

summary(UScrime)

qplot(Reg,Prob, data=UScrime)

crime.lm <- lm(Prob~., data=UScrime)

#Summary of linear regression model 
crime.lm

summary(crime.lm)

crime2.lm = lm(Prob~Reg, data=UScrime)
summary(crime2.lm)

plot(crime2.lm)
abline(crime2.lm)

par(mfrow=c(2,2)) 
plot(crime2.lm) 
par(mfrow=c(1,1))

UScrime$Reg <- factor(UScrime$So,
                      levels = c(0,1),
                      labels = c("Non-South", "South"))
qplot(Reg, Prob, data = UScrime, geom = c("boxplot","point"))


library(MASS)
set.seed(1) 
train=sample(47,24)
lm.fit=lm(Prob~Reg, data=UScrime, 
          subset=train)
attach(UScrime) 
mean((Prob-predict(lm.fit,UScrime))[-train]^2)

#k-Fold CV
library(boot)
set.seed(1)
cv.error.10=rep(0,10) 
for (i in 1:10){ 
  glm.fit=glm(Prob~poly(So,i),data=UScrime) 
  cv.error.10[i]=cv.glm(UScrime,glm.fit,K=10)$delta[1]
}
cv.error.10

#Random Forest Model
library(MASS)

library(randomForest)
set.seed(1) 
train = sample(1:nrow(UScrime), nrow(UScrime)/2) 
bag.crime = randomForest(Prob~., data=UScrime, 
                          subset=train, 
                          mtry=16, 
                          importance=TRUE) 
bag.crime

yhat.bag = predict(bag.crime, 
                   newdata=UScrime[-train,]) 
plot(yhat.bag, crime.test) 
abline(0,1)
mean((yhat.bag-crime.test)^2) 

set.seed(1) 
rf.crime = randomForest(Prob~., 
                         data=UScrime, 
                         subset=train, 
                         mtry=16, 
                         importance=TRUE) 
yhat.rf = predict(rf.crime, newdata=UScrime[-train,]) 
mean((yhat.rf-crime.test)^2) 

importance(rf.crime) 

varImpPlot(rf.crime)
