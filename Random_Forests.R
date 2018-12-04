library(MASS)
library(tree)
library(randomForest)

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