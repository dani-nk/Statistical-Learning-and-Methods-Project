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

# Test model 
set.seed(1)
split <- sample.split(visasclean$case_no, SplitRatio = 0.70)
train <- subset(visasclean, split == TRUE)
test <- subset(visasclean, split == FALSE)

# Unbalanced model - problematic because it predicts everything as certified. 
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

# Balancing the model, because an imbalanced model will always predict "certified" (right?)
# https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/
down_train <- downSample(x = train[, colnames(train) %ni% "case_status_quant"], y = train$case_status_quant, yname="case_status_quant")

# Full, balanced model 
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

# Model for country coefficients 
countrymodel <- glm(case_status_quant ~ country_of_origin + class_new + job_state_new + naics_code_new + annual_wage + decision_year, data = visasclean, family = binomial)
summary(countrymodel)

# Model with banned country dummy 
visasclean$banned<-rep(0,length(visasclean$case_no))
visasclean$banned[visasclean$country_of_origin %in% c("IRAN", "IRAQ", "LIBYA", "SOMALIA", "SUDAN", "YEMEN", "SYRIA", "CHAD", "NORTH KOREA", "VENEZUELA")]<-1
table(visasclean$banned)

split <- sample.split(visasclean$case_no, SplitRatio = 0.70)
train <- subset(visasclean, split == TRUE)
test <- subset(visasclean, split == FALSE)
down_train <- downSample(x = train[, colnames(train) %ni% "case_status_quant"], y = train$case_status_quant, yname="case_status_quant")

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

# Plotting banned country stuff? Not worth it--no statistical significance. 
banned <- as.data.frame(coef(bannedmodel)) 