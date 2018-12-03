library(MASS)
attach(UScrime)

ineq1.lm=lm(Ineq~.,data=UScrime) #linear regression for response Ineq using all other variables as predictors
summary(ineq1.lm)

# Most significant variables are Ed, GDP, and y
ineq2.lm=lm(Ineq~Ed+GDP+y,data=UScrime)
summary(ineq2.lm)
par(mfrow = c(2,2))
plot(ineq2.lm)

# Using validation set
set.seed(1)
train=sample(47,25)
ineq3.lm=lm(Ineq~.,data=UScrime,subset=train)
summary(ineq3.lm)
par(mfrow = c(2,2))
plot(ineq3.lm)



