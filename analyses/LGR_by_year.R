attach(visasclean)
library(tidyverse)

#define training set (up to the year) and test set (equal to the year) for each year 2013 on
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012)) -> year2013.train
visasclean %>% filter(decision_year %in% c(2013)) -> year2013.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,2013)) -> year2014.train
visasclean %>% filter(decision_year %in% c(2014)) -> year2014.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014)) -> year2015.train
visasclean %>% filter(decision_year %in% c(2015)) -> year2015.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015)) -> year2016.train
visasclean %>% filter(decision_year %in% c(2016)) -> year2016.test
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015,2016)) -> year2017.train
visasclean %>% filter(decision_year %in% c(2017)) -> year2017.test

#2013
glm.2013<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+as.factor(naics_code_new)+as.factor(country_new),data = year2013.train)
glm.probs.2013=predict(glm.2013,newdata=year2013.test,type="response")

glm.pred.2013=rep("DENIED",30715)
glm.pred.2013[glm.probs.2013 >.5]="CERTIFIED"
table(glm.pred.2013,year2013.test$case_status_new)
mean(glm.pred.2013!=year2013.test$case_status_new) #10.975% test error
#Type 1 error (false positive -- certified as denied)
67/30715 #0.218%
#Type 2 error (false negative -- denied as certified)
3304/30715 #10.75%)

#2014
glm.2014<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+as.factor(naics_code_new)+as.factor(country_new),data = year2014.train)
glm.probs.2014=predict(glm.2014,newdata=year2014.test,type="response")

glm.pred.2014=rep("DENIED",63992)
glm.pred.2014[glm.probs.2014 >.5]="CERTIFIED"
table(glm.pred.2014,year2014.test$case_status_new)
mean(glm.pred.2014!=year2014.test$case_status_new) #5.54% test error
#Type 1 error (false positive -- certified as denied)
84/63992 #0.131%
#Type 2 error (false negative -- denied as certified)
3461/63992 #5.41%

#2015
glm.2015<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+as.factor(naics_code_new)+as.factor(country_new),data = year2015.train)
glm.probs.2015=predict(glm.2015,newdata=year2015.test,type="response")

glm.pred.2015=rep("DENIED",79600)
glm.pred.2015[glm.probs.2015 >.5]="CERTIFIED"
table(glm.pred.2015,year2015.test$case_status_new)
mean(glm.pred.2015!=year2015.test$case_status_new) #5.53% test error
#Type 1 error (false positive -- certified as denied)
147/79600 #0.185%
#Type 2 error (false negative -- denied as certified)
4254/79600 #5.34%)

#2016
glm.2016<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+as.factor(naics_code_new)+as.factor(country_new),data = year2016.train)
glm.probs.2016=predict(glm.2016,newdata=year2016.test,type="response")

glm.pred.2016=rep("DENIED",83234)
glm.pred.2016[glm.probs.2016 >.5]="CERTIFIED"
table(glm.pred.2016,year2016.test$case_status_new)
mean(glm.pred.2016!=year2016.test$case_status_new) #3.85% test error
#Type 1 error (false positive -- certified as denied)
61/83234 #0.073%
#Type 2 error (false negative -- denied as certified)
3143/83234 #3.78%)

#2017
glm.2017<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+as.factor(naics_code_new)+as.factor(country_new),data = year2017.train)
glm.probs.2017=predict(glm.2017,newdata=year2017.test,type="response")

glm.pred.2017=rep("DENIED",16218)
glm.pred.2017[glm.probs.2017 >.5]="CERTIFIED"
table(glm.pred.2017,year2017.test$case_status_new)
mean(glm.pred.2017!=year2017.test$case_status_new) #9.56% test error
#Type 1 error (false positive -- certified as denied)
15/16218 #0.092%
#Type 2 error (false negative -- denied as certified)
1538/16218 #9.48%)

