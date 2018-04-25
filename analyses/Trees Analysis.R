visasclean <- read.csv("~/Documents/GitHub/visasclean.csv")

visasclean$decision_year <- as.numeric(substr(as.character(visasclean$decision_date), 1, 4))
visasclean$decision_year <- as.factor(visasclean$decision_year)

visasclean %>% mutate(country_new = 
                        ifelse(country_of_origin == "INDIA", "INDIA", 
                               ifelse(country_of_origin == "CHINA", "CHINA", 
                                      ifelse(country_of_origin == "SOUTH KOREA", "SOUTH KOREA", 
                                             ifelse(country_of_origin == "CANADA", "CANADA", 
                                                    ifelse(country_of_origin == "MEXICO", "MEXICO", 
                                                           ifelse(country_of_origin == "PHILIPPINES", "PHILIPPINES", 
                                                                  ifelse(country_of_origin == "UNITED KINGDOM", "UNITED KINGDOM", 
                                                                         ifelse(country_of_origin == "TAIWAN", "TAIWAN", 
                                                                                ifelse(country_of_origin == "PAKISTAN", "PAKISTAN", 
                                                                                       ifelse(country_of_origin == "BRAZIL", "BRAZIL",          
                                                                                              "OTHER"))))))))))) -> visasclean

topten_class_of_admission <-table(visasclean$class_of_admission)
View(topten_class_of_admission)
visasclean %>% mutate(class_new = 
                        ifelse(visasclean$class_of_admission == "H-1B", "H-1B", 
                               ifelse(visasclean$class_of_admission == "L-1", "L-1", 
                                      ifelse(visasclean$class_of_admission == "F-1", "F-1", 
                                             ifelse(visasclean$class_of_admission == "B-2", "B-2", 
                                                    ifelse(visasclean$class_of_admission == "Not in USA", "Not in USA", 
                                                           ifelse(visasclean$class_of_admission == "EWI", "EWI", 
                                                                  ifelse(visasclean$class_of_admission == "E-2", "E-2", 
                                                                         ifelse(visasclean$class_of_admission == "TN", "TN", 
                                                                                ifelse(visasclean$class_of_admission == "Parolee", "Parolee", 
                                                                                       ifelse(visasclean$class_of_admission == "H-1B1", "H-1B1",          
                                                                                              "OTHER"))))))))))) -> visasclean
visasclean %>% mutate(job_state_new = 
                        ifelse(visasclean$job_state == "CA", "CA", 
                               ifelse(visasclean$job_state == "TX", "TX", 
                                      ifelse(visasclean$job_state == "NJ", "NJ", 
                                             ifelse(visasclean$job_state == "NY", "NY", 
                                                    ifelse(visasclean$job_state == "WA", "WA", 
                                                           ifelse(visasclean$job_state == "IL", "IL", 
                                                                  ifelse(visasclean$job_state == "FL", "FL", 
                                                                         ifelse(visasclean$job_state == "VA", "VA", 
                                                                                ifelse(visasclean$job_state == "MA", "MA", 
                                                                                       ifelse(visasclean$job_state == "GA", "GA", ifelse(visasclean$job_state == "MI", "MI", ifelse(visasclean$job_state == "PA", "PA", ifelse(visasclean$job_state == "NC", "NC", ifelse(visasclean$job_state == "MD", "MD", ifelse(visasclean$job_state == "OH", "OH",         
                                                                                                                                                                                                                                                                                                                     "OTHER")))))))))))))))) -> visasclean

visasclean$country_new <- as.factor(visasclean$country_new)
visasclean$class_new <- as.factor(visasclean$class_new)
visasclean$job_state_new <- as.factor(visasclean$job_state_new)

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
#Not the right way to test result - come back to this

#Now try random forests
library(randomForest)

set.seed(100)
rf.visas <- randomForest(case_status_new~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+naics_code_new+decision_year+country_new+class_new+job_state_new,data=train,mtry=3,importance=TRUE, ntree=20, na.action=na.exclude)
rf.visas

#Do test set
yhat.rf <- predict(rf.visas,newdata = test)
table(yhat.rf,test$case_status_new)
mean(yhat.rf==test$case_status_new)
importance(rf.visas)
