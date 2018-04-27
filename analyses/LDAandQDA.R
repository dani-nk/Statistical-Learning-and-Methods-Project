
#define test and training set
set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,] 

#LDA
library(MASS)
attach(visasclean)
lda<-lda(case_status_new~annual_wage+decision_year+as.factor(country_new)+as.factor(naics_code_new)+as.factor(job_state_new), visasclean.train)

#prediction
lda.prediction<-predict(lda,visasclean.test)
detach(visasclean)
names(lda.prediction)
lda.class=lda.prediction$class
table(lda.class,visasclean.test$case_status_new)
mean(lda.class==visasclean.test$case_status_new)
mean(lda.class!=visasclean.test$case_status_new)

#add in more variables, removing country_new because of collinearity with other country data
attach(visasclean)
lda2<-lda(case_status_new~annual_wage+decision_year+as.factor(naics_code_new)+as.factor(class_new)+as.factor(job_state_new)+gdp_per_cap+pop_muslim+migration_per_cap+unemployment,visasclean.train)
lda.prediction2<-predict(lda2,visasclean.test)
detach(visasclean)
names(lda.prediction2)
lda.class2=lda.prediction2$class
table(lda.class2,visasclean.test$case_status_new)
mean(lda.class2==visasclean.test$case_status_new)
mean(lda.class2!=visasclean.test$case_status_new)
#Type 1 error (false positive -- certified as denied)
1751/154357 #1.1%
#Type 2 error (false negative -- denied as certified)
10819/154357 #7%

#QDA
attach(visasclean)
qda<-qda(case_status_new~annual_wage+decision_year+as.factor(naics_code_new)+as.factor(class_new)+as.factor(job_state_new)+gdp_per_cap+pop_muslim+migration_per_cap+unemployment,visasclean.train)
detach(visasclean)
qda
qda.prediction<-predict(qda,visasclean.test)
qda.class=qda.prediction$class
table(qda.class,visasclean.test$case_status_new)
mean(qda.class==visasclean.test$case_status_new)
mean(qda.class!=visasclean.test$case_status_new)
#Type 1 error (false positive -- certified as denied)
22887/154357 #14.8%
#Type 2 error (false negative -- denied as certified)
7178/154357 #4.65%
