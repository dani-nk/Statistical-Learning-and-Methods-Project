View(visasclean)
# delete observaitons visasclean[country_of_origin== "COMOROS" | "EQUATORIAL GUINEA" | "SOLOMON ISLANDS"]
#LDA
#define test and training set
set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,] 

#part b#
library(MASS)
attach(visasclean)
lda<-lda(case_status_new~annual_wage,visasclean.train)
lda2<-lda(case_status_new~annual_wage+as.factor(country_new)+as.factor(naics_code_new)+as.factor(job_state_new), visasclean.train)
lda3<-lda(case_status_new~annual_wage+as.factor(naics_code_new)+as.factor(class_new)+as.factor(job_state_new)+gdp_per_cap+pop_muslim+migration_per_cap+unemployment)
lda.prediction<-predict(lda2,visasclean.test)

warnings()

#names(lda.prediction)
#lda.class=lda.prediction$class
#table(lda.class,case_status_new)
#(206+207)/1599
#mean(lda.class==case_status_new)
#mean(lda.class!=case_status_new)