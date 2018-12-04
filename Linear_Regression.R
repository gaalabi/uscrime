library(MASS)
attach(UScrime)

pairs(UScrime[,])

ineq1.lm=lm(Ineq~.,data=UScrime) #linear regression for response Ineq using all other variables as predictors
summary(ineq1.lm)
par(mfrow = c(2,2))
plot(ineq1.lm)

step(ineq1.lm)

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



