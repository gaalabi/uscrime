library(MASS)
attach(UScrime)

library(tree)
library(randomForest)

# Linear Regression
pairs(UScrime[,])

ineq1.lm=lm(Ineq~.,data=UScrime) #linear regression for response Ineq using all other variables as predictors
summary(ineq1.lm)
par(mfrow = c(2,2))
plot(ineq1.lm)



# Most significant variables are Ed, GDP, and y
ineq2.lm=lm(Ineq~Ed+GDP+y,data=UScrime)
summary(ineq2.lm)
par(mfrow = c(2,2))
plot(ineq2.lm)

# k-fold validation
library(boot)
set.seed(1)
#train=sample(47,25)
ineq.fit=glm(Ineq~.,data=UScrime)
ineq2.fit=glm(Ineq~GDP+Ed+y,data=UScrime)

cv.errors=cv.glm(UScrime,ineq.fit,K=10)$delta
cv.err=cv.errors[1]
cv.err

cv.errors2=cv.glm(UScrime,ineq2.fit,K=10)$delta
cv.err2=cv.errors2[1]
cv.err2

# Random Forests

B=1000 
n=nrow(UScrime) # total number of data points
p=ncol(UScrime)-1 # number of predictors

#train/test subset
set.seed(1)
train=sample(1:nrow(UScrime), nrow(UScrime)/2)
UScrime.test=subset(UScrime[-train,],select=-Ineq) 
ineq.test=UScrime$Ineq[-train]

# creating random forests
rf.ineq=randomForest(Ineq~.,data=UScrime,subset=train,
                     xtest=UScrime.test,ytest=ineq.test,
                     ntree=B,mtry=sqrt(p),importance=T)
#Test MSE is here
rf.ineq

#Predicted values from random forest
rf.ineq$predicted

# Which predictors are most important?
importance(rf.ineq)
varImpPlot(rf.ineq)
