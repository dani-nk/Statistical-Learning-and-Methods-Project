visasclean <- read.csv("~/Documents/GitHub/visasclean.csv")

visasclean$decision_year <- as.numeric(substr(as.character(visasclean$decision_date), 1, 4))
visasclean$decision_year <- as.factor(visasclean$decision_year)

#Recode class_of_admission and country_of_origin
library(tidyverse)
library(tree)
attach(visasclean)
case_status_new <- as.factor(case_status_new)

#Create training and test set
require(caTools)
set.seed(1)
sample = sample.split(visasclean$case_no, SplitRatio = .70)
train = subset(visasclean, sample == TRUE)
test  = subset(visasclean, sample == FALSE)

#Try classification tree
tree1 <- tree(case_status_new~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+naics_code_new+decision_year,train)
summary(tree1)
plot(tree1)
text(tree1,pretty=0)
#Everything is classified as certified. In order to get at differences I have to do quant as below

#Run a quantitative tree
tree2 <- tree(case_status_quant~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+naics_code_new+decision_year,train)
summary(tree2)
plot(tree2)
text(tree2,pretty=0)

#Can't get pruning to work using this method. But also, pruning doesn't seem that useful
cv.tree2 <- cv.tree(tree2)
plot(cv.tree2,cv.tree2$dev,type='b')

#Check how it performs on the test set
yhat=predict(tree2, newdata=test)
plot(yhat,test$case_status_quant)
abline(0,1)
mean((yhat-test$case_status_quant)^2)
