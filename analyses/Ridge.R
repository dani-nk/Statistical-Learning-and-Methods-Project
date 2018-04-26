## Using this online tutorial: https://drsimonj.svbtle.com/ridge-regression-with-glmnet

library(tidyverse)
library(broom)
library(glmnet)

# define test and training set
set.seed(1)
train <- sample(1:nrow(visasclean), 0.5*nrow(visasclean)) #i was getting an error that the test and training sets were different lengths and didn't know how to fix so just made them 50/50
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,] 

# set up ridge 
y <- visasclean.train$case_status_quant
x <- visasclean.train %>% select(as.factor(class_new),as.factor(country_new),naics_code_new,annual_wage,as.factor(job_state_new),gdp_per_cap,pop_muslim,migration_per_cap,unemployment) %>% data.matrix()
#generates 5 warnings
suppressWarnings(x)
#it's pulling in all the variables we dont' want for some reason and setting them to NA. excluding them from the dataframe
visasclean.train %>% select(-country_of_origin,-employer_state,-class_of_admission,-naics_title,-decision_date,-employer_zip,-case_no,-employer_name,-naics_code,-employer_city,-pw_soc_code,-pw_level,-pw_job_title,-pw_unit_of_pay) -> visasclean
#still getting 5 error messages -- none of the factor conversions are working. trying only with numeric variables. 
x <- visasclean.train %>% select(naics_code_new,annual_wage,gdp_per_cap,pop_muslim,migration_per_cap,unemployment) %>% data.matrix()

## optional: define your own lambdas (according to the internet) lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y, alpha = 0)
summary(fit)

# get optimal lambda (lowest MSE point on curve)
cv_fit <- cv.glmnet(x, y, alpha = 0)
plot(cv_fit)
# extract optimal lambda from plot
opt_lambda <- cv_fit$lambda.min
opt_lambda

# extract all of the fitted models
fit <- cv_fit$glmnet.fit
summary(fit)

# predicting values and computing an R2 value for the data we trained on
x.test <- visasclean.test %>% select(naics_code_new,annual_wage,gdp_per_cap,pop_muslim,migration_per_cap,unemployment) %>% data.matrix()
y_predicted <- predict(fit, s = opt_lambda, newx = x.test)

# MSE
mean((y_predicted-y)^2) 
  # 7.23%

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq 
  # 0.0027
  # this only explains .27% of the variation in the data it looks like ... 