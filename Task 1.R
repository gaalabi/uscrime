library(randomForest)
library(MASS)
library(boot)

attach(UScrime)

crim <- MASS::UScrime

n <- nrow(crim)
p <- ncol(crim)-1
mte <- 0

B <- 1000

set.seed(1)

for(iter in 1:10){
  train <- sample(n, 0.8*n)
  crim.test <- subset(crim[-train,], select=-y)
  y.test = crim$y[-train]
  
  randf <- randomForest(y ~., data=crim, subset=train, xtest = crim.test, ytest = y.test, ntree = B, mtry = sqrt(p), importance=T)
  
  mte <- mte + randf$mse
}

#Average test prediction error

mte <- mte/10
mean(mte)

#Most important varialbes
varImpPlot(randf)


#Looking for interactions between predictors
pairs(crim)



#Model

included <- names(crim)[-15]

for(iter in 1:10){
  train <- sample(n, 0.8*n)
  crim.glm = glm(y~.+U1:U2+Po1+Po2+GDP:Ineq, data=crim[,included])
  
  cv.err <- cv.glm(crim,crim.glm)$delta
  cv.errsum <- cv.errsum + cv.err
  
}
#LOOV, as we only have 47 observations and I wanted to use all of them.
cv.errsum <- cv.errsum/10
cv.errsum <-cv.errsum[1]
cv.errsum


train <- sample(n, 0.8*n)
crim.lm = lm(y~.+Po1+Po2, data=crim[,included], subset = train)

#Checking conditions
resid.crim = resid(crim.lm)
qqnorm(resid.crim)
qqline(resid.crim)

plot(crim.lm)

#Most important variables 
step(crim.lm)



