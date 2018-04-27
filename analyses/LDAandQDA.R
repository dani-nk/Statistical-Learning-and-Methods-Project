
#define test and training set
set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,]

#LDA (using country_new instead of other collinear country predictors)
library(MASS)
attach(visasclean)
lda<-lda(case_status_new~annual_wage+decision_year+as.factor(country_new)+as.factor(naics_code_new)+as.factor(job_state_new), visasclean.train)
lda

#prediction
lda.prediction<-predict(lda,visasclean.test)
detach(visasclean)
names(lda.prediction)
lda.class=lda.prediction$class
table(lda.class,visasclean.test$case_status_new)
mean(lda.class==visasclean.test$case_status_new)
mean(lda.class!=visasclean.test$case_status_new) # 8.35% test error rate

#Type 1 error (false positive -- certified as denied)
1895/154357 #1.22%
#Type 2 error (false negative -- denied as certified)
10999/154357 #7.13%

#QDA
attach(visasclean)
qda<-qda(case_status_new~annual_wage+decision_year+as.factor(naics_code_new)+as.factor(class_new)+as.factor(job_state_new)+as.factor(country_new),visasclean.train)
detach(visasclean)
qda
qda.prediction<-predict(qda,visasclean.test)
qda.class=qda.prediction$class
table(qda.class,visasclean.test$case_status_new)
mean(qda.class==visasclean.test$case_status_new)
mean(qda.class!=visasclean.test$case_status_new) #20.43% test error
#Type 1 error (false positive -- certified as denied)
24648/154357 #15.87%
#Type 2 error (false negative -- denied as certified)
6884/154357 #4.46%
