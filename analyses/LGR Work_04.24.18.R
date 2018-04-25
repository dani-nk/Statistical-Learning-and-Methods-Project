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
glm424<-glm(case_status_quant~as.factor(class_new)+as.factor(country_new)+as.factor(job_state)+annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+population+as.factor(naics_code_new),data = visasclean.train)
topten_job_state <-table(visasclean$job_state)
view(topten_job_state)
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          +                                                                                                 "OTHER")))))))))))))))) -> visasclean
set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,]
glm424<-glm(case_status_quant~as.factor(class_new)+as.factor(job_state_new)+annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+population+as.factor(naics_code_new),data = visasclean.train)
summary(glm424)
glm.probs.424=predict(glm424,visasclean.test,type="response")
glm.pred.424=rep("DENIED",217270)
glm.pred.424[glm.probs.424 >.5]="CERTIFIED"
table(glm.pred.424,visasclean.test$case_status_new)
mean(glm.pred.424!=visasclean.test$case_status_new)
glm.pred.424.7=rep("DENIED",217270)
glm.pred.424.7[glm.probs.424 >.7]="CERTIFIED"
table(glm.pred.424.7,visasclean.test$case_status_new)
mean(glm.pred.424.7!=visasclean.test$case_status_new)
glm.pred.424.9=rep("DENIED",217270)
glm.pred.424.9[glm.probs.424 >.9]="CERTIFIED"
table(glm.pred.424.9,visasclean.test$case_status_new)
mean(glm.pred.424.9!=visasclean.test$case_status_new)
glm.pred.424.1=rep("DENIED",217270)
glm.pred.424.1[glm.probs.424 >.1]="CERTIFIED"
table(glm.pred.424.1,visasclean.test$case_status_new)
mean(glm.pred.424.1!=visasclean.test$case_status_new)
glm.pred.424.3=rep("DENIED",217270)
glm.pred.424.3[glm.probs.424 >.3]="CERTIFIED"
table(glm.pred.424.3,visasclean.test$case_status_new)
mean(glm.pred.424.3!=visasclean.test$case_status_new)