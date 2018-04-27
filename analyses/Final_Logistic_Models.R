
##### Libraries and cleaning #####
library(tidyverse)
library(readxl)
library(caTools)
library(caret)
library(lubridate)
library(glmnet)
'%ni%' <- Negate('%in%')

visasclean %>%
  mutate(naics_code = as.factor(naics_code),
         class_of_admission = as.factor(class_of_admission),
         employer_zip = as.factor(employer_zip),
         naics_code_new = as.factor(naics_code_new),
         pw_level = as.factor(pw_level),
         case_status = as.factor(case_status),
         case_status_new = as.factor(case_status_new),
         job_state = as.factor(job_state),
         employer_state = as.factor(employer_state),
         pw_unit_of_pay = as.factor(pw_unit_of_pay),
         pw_soc_code = as.factor(pw_soc_code),
         population = as.numeric(population),
         case_status_quant = as.factor(case_status_quant),
         country_of_origin = as.factor(country_of_origin)) -> visasclean

visasclean$banned<-rep(0,length(visasclean$case_no))
visasclean$banned[visasclean$country_of_origin %in% c("IRAN", "IRAQ", "LIBYA", "SOMALIA", "SUDAN", "YEMEN", "SYRIA", "CHAD", "NORTH KOREA", "VENEZUELA")]<-1

##### Splitting model #####
set.seed(1)
split <- sample.split(visasclean$case_no, SplitRatio = 0.70)
train <- subset(visasclean, split == TRUE)
test <- subset(visasclean, split == FALSE)

##### Imbalanced model - problematic because it predicts everything as certified. #####
model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim + decision_year, data = train, family = binomial)
summary(model)
pred <- predict(model, newdata = test, type = "response", na.action=)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$case_status_quant
table(y_pred, y_act)
#             y_act
# y_pred      0      1
# 0           797    643
# 1           11140 133379

# Test error 
mean(y_pred!=y_act) # 8.07%

# Type 1 error (false positive -- certified as denied)
11140/(797+11140) # 93.3% 
# Type 2 error (false negative -- denied as certified)
643/(643+133379) # 0.48% 

##### Balancing the model, because an imbalanced model will always predict "certified" (right?) #####
# https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/
down_train <- downSample(x = train[, colnames(train) %ni% "case_status_quant"], y = train$case_status_quant, yname="case_status_quant")

##### Full, balanced model #####
bigmodel <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim + decision_year, data = down_train, family = binomial)
summary(bigmodel)
pred.big <- predict(bigmodel, newdata = test, type = "response", na.action=)
y_pred_num.big <- ifelse(pred.big > 0.5, 1, 0)
y_pred.big <- factor(y_pred_num.big, levels=c(0, 1))
y_act.big <- test$case_status_quant
table(y_pred.big, y_act.big)
#              y_act.big
# y_pred.big     0     1
# 0           7182 39669
# 1           4755 94353

# Test error 
mean(y_pred.big!=y_act.big) # 30.4%
# Type 1 error (false positive -- certified as denied)
4755/(4755+7182) # 39.8% 
# Type 2 error (false negative -- denied as certified)
39669/(39669+94353) # 29.6%

##### Model for extracting country coefficients, for Amie #####
countrymodel <- glm(case_status_quant ~ country_of_origin + class_new + job_state_new + naics_code_new + annual_wage + decision_year, data = visasclean, family = binomial)
summary(countrymodel)
coef <- as.data.frame(coef(countrymodel))

##### Model with banned country dummy - #fail. No effect. Model not better. #####
bannedmodel <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + banned*decision_year, data = down_train, family = binomial)
summary(bannedmodel)

pred.banned <- predict(bannedmodel, newdata = test, type = "response", na.action=)
y_pred_num.banned <- ifelse(pred.banned > 0.5, 1, 0)
y_pred.banned <- factor(y_pred_num.banned, levels=c(0, 1))
y_act.banned <- test$case_status_quant
table(y_pred.banned, y_act.banned)
#                   y_act.banned
# y_pred.banned     0     1
# 0                 7270 39464
# 1                 4578 94647

# Test error 
mean(y_pred.banned!=y_act.banned) # 30.8%
# Type 1 error (false positive -- certified as denied)
4578/(4578+7270) # % 38.6%
# Type 2 error (false negative -- denied as certified)
39464/(39464+94647) # 29.4%

##### Plotting banned country stuff? Not worth it--no statistical significance. #####

##### Define training set (up to the year) and test set (equal to the year) for each year 2013 on ####

##### 2013 #####
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012)) -> year2013.train
visasclean %>% filter(decision_year %in% c(2013)) -> year2013.test
year2013.down_train <- downSample(x = year2013.train[, colnames(year2013.train) %ni% "case_status_quant"], y = year2013.train$case_status_quant, yname="case_status_quant")

year2013model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim, data = year2013.down_train, family = binomial)
summary(year2013model)
pred.year2013 <- predict(year2013model, newdata = year2013.test, type = "response", na.action=)
y_pred_num.year2013 <- ifelse(pred.year2013 > 0.5, 1, 0)
y_pred.year2013 <- factor(y_pred_num.year2013, levels=c(0, 1))
y_act.year2013 <- year2013.test$case_status_quant
table(y_pred.year2013, y_act.year2013)
#                     y_act.year2013
# y_pred.year2013     0     1
# 0                   830  3672
# 1                   2541 23672

# Test error 
mean(y_pred.year2013!=y_act.year2013) # 20.2% 
# Type 1 error (false positive -- actual denied, predicted certified)
2541/(2541+830) # 75.3% 
# Type 2 error (false negative -- actual certified, predicted denied)
3672/(3672+23672) # 13.4%

##### 2014 #####
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,2013)) -> year2014.train
visasclean %>% filter(decision_year %in% c(2014)) -> year2014.test
year2014.down_train <- downSample(x = year2014.train[, colnames(year2014.train) %ni% "case_status_quant"], y = year2014.train$case_status_quant, yname="case_status_quant")
year2014model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim, data = year2014.down_train, family = binomial)
summary(year2014model)
pred.year2014 <- predict(year2014model, newdata = year2014.test, type = "response", na.action=)
y_pred_num.year2014 <- ifelse(pred.year2014 > 0.5, 1, 0)
y_pred.year2014 <- factor(y_pred_num.year2014, levels=c(0, 1))
y_act.year2014 <- year2014.test$case_status_quant
table(y_pred.year2014, y_act.year2014)
#                     y_act.year2014
# y_pred.year2014     0     1
# 0                   1030  8275
# 1                   2486 52201

# Test error 
mean(y_pred.year2014!=y_act.year2014) # 16.8% 
# Type 1 error (false positive -- actual denied, predicted certified)
2486/(2486+1030) # 70.7% 
# Type 2 error (false negative -- actual certified, predicted denied)
8275/(52201+8275) # 13.6%

##### 2015 #####
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014)) -> year2015.train
visasclean %>% filter(decision_year %in% c(2015)) -> year2015.test
year2015.down_train <- downSample(x = year2015.train[, colnames(year2015.train) %ni% "case_status_quant"], y = year2015.train$case_status_quant, yname="case_status_quant")
year2015model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim, data = year2015.down_train, family = binomial)
summary(year2015model)
pred.year2015 <- predict(year2015model, newdata = year2015.test, type = "response", na.action=)
y_pred_num.year2015 <- ifelse(pred.year2015 > 0.5, 1, 0)
y_pred.year2015 <- factor(y_pred_num.year2015, levels=c(0, 1))
y_act.year2015 <- year2015.test$case_status_quant
table(y_pred.year2015, y_act.year2015)
#                     y_act.year2015
# y_pred.year2015     0     1
# 0                   1529 11880
# 1                   2784 63407
# Test error 
mean(y_pred.year2015!=y_act.year2015) # 18.4%
# Type 1 error (false positive -- actual denied, predicted certified)
2784/(2784+1529)
# Type 2 error (false negative -- actual certified, predicted denied)
11880/(11880+63407)

##### 2016 ##### 
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015)) -> year2016.train
visasclean %>% filter(decision_year %in% c(2016)) -> year2016.test
year2016.down_train <- downSample(x = year2016.train[, colnames(year2016.train) %ni% "case_status_quant"], y = year2016.train$case_status_quant, yname="case_status_quant")
year2016model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim, data = year2016.down_train, family = binomial)
summary(year2016model)
pred.year2016 <- predict(year2016model, newdata = year2016.test, type = "response", na.action=)
y_pred_num.year2016 <- ifelse(pred.year2016 > 0.5, 1, 0)
y_pred.year2016 <- factor(y_pred_num.year2016, levels=c(0, 1))
y_act.year2016 <- year2016.test$case_status_quant
table(y_pred.year2016, y_act.year2016)
#                     y_act.year2016
# y_pred.year2016     0     1
# 0                   1418 15244
# 1                   1764 64808
# Test error 
mean(y_pred.year2016!=y_act.year2016) # 20.4% 
# Type 1 error (false positive -- actual denied, predicted certified)
1764/(1764+1418)
# Type 2 error (false negative -- actual certified, predicted denied)
15244/(15244+64808)

##### 2017 ##### 
visasclean %>% filter(decision_year %in% c(2008,2009,2010,2011,2012,20133,2014,2015,2016)) -> year2017.train
visasclean %>% filter(decision_year %in% c(2017)) -> year2017.test
year2017.down_train <- downSample(x = year2017.train[, colnames(year2017.train) %ni% "case_status_quant"], y = year2017.train$case_status_quant, yname="case_status_quant")
year2017model <- glm(case_status_quant ~ class_new + job_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim, data = year2017.down_train, family = binomial)
summary(year2017model)
pred.year2017 <- predict(year2017model, newdata = year2017.test, type = "response", na.action=)
y_pred_num.year2017 <- ifelse(pred.year2017 > 0.5, 1, 0)
y_pred.year2017 <- factor(y_pred_num.year2017, levels=c(0, 1))
y_act.year2017 <- year2017.test$case_status_quant
table(y_pred.year2017, y_act.year2017)
#                     y_act.year2017
# y_pred.year2017     0     1
# 0                   1076  4037
# 1                   475 10630
# Test error 
mean(y_pred.year2017!=y_act.year2017) #27.8%
# Type 1 error (false positive -- actual denied, predicted certified)
475/(475+1076)
# Type 2 error (false negative -- actual certified, predicted denied)
4037/(4037+10630)
