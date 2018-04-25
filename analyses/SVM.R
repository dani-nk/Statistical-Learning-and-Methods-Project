# Replicating p. 359 in ISL.

library(tidyverse) # for everything 
library(e1071)
library(lubridate)
visas <- read_csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/visasclean.csv")
visas %>% mutate(Year = year(decision_date)) -> visas
visas %>% filter(Year==2016) -> visas2016

set.seed (1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit , dat)
svmfit$index
summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

# "kernel=" allows for non-linear decision boundaries. 
svmfit=svm(case_status_quant~pop_muslim, data=visas2016, kernel="linear", cost=10, scale=FALSE)
plot(svmfit , dat)
svmfit$index
summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)
