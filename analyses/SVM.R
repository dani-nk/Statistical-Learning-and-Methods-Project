# Replicating p. 359 in ISL.

library(tidyverse) # for everything 
library(e1071)
library(lubridate)
visas <- read_csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/visasclean.csv")

# Subsetting data, because every time I run svm() RStudio crashes. 
visas %>% mutate(Year = year(decision_date), 
                 Month = month(decision_date),
                 Day = day(decision_date)) -> visas
ggplot(visas) + geom_bar(aes(Month))
visas %>% filter(Year==2016 & Month==5) -> visasMay2016
ggplot(visasMay2016) + geom_bar(aes(Day))
visas %>% filter(Day == 11) -> visas2016May11

# Practice svm() from ISL 
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
# Note: "kernel=polynomial" and "kernel=radial" allow for non-linear decision boundaries. 


# Trying with real data, but can't get a subset small enough! 
# "WARNING: reaching max number of iterations"
svmfit=svm(case_status_quant~pop_muslim, data=visasMay2016, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, visasMay2016)
svmfit$index
summary(svmfit)

tune.out=tune(data=visasMay2016,case_status_quant ~ pop_muslim + unemployment,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))  
# Error in is.vector(train.x) : argument "train.x" is missing, with no default