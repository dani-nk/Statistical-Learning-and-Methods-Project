# mostly following along with https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/

library(tidyverse)
library(readxl)
library(caTools)
library(caret)
library(lubridate)
library(glmnet)
'%ni%' <- Negate('%in%')
visas <- read_csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/visasclean.csv")
visas %>% 
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
         country_of_origin = as.factor(country_of_origin)) -> visas

visas %>% mutate(class_new = 
         ifelse(class_of_admission == "H-1B", "H-1B", 
         ifelse(class_of_admission == "L-1", "L-1", 
         ifelse(class_of_admission == "F-1", "F-1", 
         ifelse(class_of_admission == "B-2", "B-2", 
         ifelse(class_of_admission == "Not in USA", "Not in USA", 
         ifelse(class_of_admission == "EWI", "EWI", 
         ifelse(class_of_admission == "E-2", "E-2", 
         ifelse(class_of_admission == "TN", "TN", 
         ifelse(class_of_admission == "Parolee", "Parolee", 
         ifelse(class_of_admission == "H-1B1", "H-1B1",          
         "OTHER"))))))))))) -> visas

visas %>% 
  mutate(emp_state_new = 
           replace(employer_state, employer_state=="NORTHERN MARIANA ISLANDS" | 
              employer_state=="MARSHALL ISLANDS" | 
              employer_state=="BRITISH COLUMBIA", 
                "OTHER")) -> visas
# Not quite, but it'll do for now. 

visas %>% mutate(Year = as.factor(year(decision_date))) -> visas

# Test model 
set.seed(1)
split <- sample.split(visas$case_no, SplitRatio = 0.70)
train <- subset(visas, split == TRUE)
test <- subset(visas, split == FALSE)
# model <- glm (case_status_quant ~ annual_wage + pop_muslim + gdp_per_cap, data = visas, family = binomial)
# summary(model)

# Balanced model 
down_train <- downSample(x = train[, colnames(train) %ni% "case_status_quant"],
                         y = train$case_status_quant,
                         yname="case_status_quant")
table(down_train$case_status_quant)
# balancemodel <- glm(case_status_quant ~ annual_wage + pop_muslim + gdp_per_cap, data = down_train, family = binomial)
# summary(balancemodel)

# pred <- predict(balancemodel, newdata = test, type = "response")
# y_pred_num <- ifelse(pred > 0.5, 1, 0)
# y_pred <- factor(y_pred_num, levels=c(0, 1))
# y_act <- test$case_status_quant
# mean(y_pred == y_act)
# table(y_pred, y_act)
# mean(y_pred==y_act, na.rm = T) [0.591]

# Full model 
bigmodel <- glm(case_status_quant ~ class_new + emp_state_new + naics_code_new + annual_wage + gdp_per_cap + unemployment + net_migration + migration_per_cap + pop_muslim + Year, data = down_train, family = binomial)
summary(bigmodel)

pred.big <- predict(bigmodel, newdata = test, type = "response", na.action=)
y_pred_num.big <- ifelse(pred.big > 0.5, 1, 0)
y_pred.big <- factor(y_pred_num.big, levels=c(0, 1))
y_act.big <- test$case_status_quant
table(y_pred.big, y_act.big)
mean(y_pred.big==y_act.big, na.rm = T) # 0.798

# HOUSTON WE HAVE A MODEL 
# y_act.big
# y_pred.big      0      1
# 0               7609  38495
# 1               7598 143590
# [1] 0.7663717 