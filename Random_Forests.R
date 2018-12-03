library(MASS)
library(tree)
attach(UScrime)
library(randomForest)

set.seed(1)
train=sample(1:nrow(UScrime),nrow(UScrime)/2)
tree.ineq=tree(Ineq~.,data=UScrime, subset=train)
summary(tree.ineq)
par(mfrow = c(1,1))
plot(tree.ineq)
text(tree.ineq,pretty=0)
ineq.test=UScrime[-train,"Ineq"]

rf.ineq=randomForest(Ineq~.,data=UScrime,subset=train,mtry=4,importance=TRUE)
yhat.rf=predict(rf.ineq,newdata=UScrime[-train,])
mean((yhat.rf-ineq.test)^2)
importance(rf.ineq)
varImpPlot(rf.ineq)