View(visasclean)
set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,]
glm420<-glm(case_status_quant~as.factor(class_of_admission)+job_state+annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+population+as.factor(naics_code_new),data = visasclean)
summary(glm420)
glm.probs.420=predict(glm420,visasclean.test,type="response")
glm.pred.420=rep("DENIED",217270)
glm.pred.420[glm.probs.420 >.5]="CERTIFIED"
table(glm.pred.420,visasclean.test$case_status_new)
mean(glm.pred.420!=visasclean.test$case_status_new)
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
ggplot(visasclean, aes(year)) + geom_bar(data=subset(visasclean,country_of_origin=="IRAN"),stat="count", aes(fill=case_status))

