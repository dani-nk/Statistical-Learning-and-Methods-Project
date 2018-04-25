# Attempting to follow pp.155-? in ISL. 
library(tidyverse) # for everything 
library(class) # for KNN 
visasclean <- read_csv("~/Documents/GitHub/visasclean.csv")
# attach(visasclean) # What does this do? 

# Changing data types
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
         case_status_quant = as.numeric(case_status_quant),
         country_of_origin = as.factor(country_of_origin)) -> visasclean

# Removing wage_offer_to because of the number of NAs. 
visasclean %>% select(-wage_offer_to) -> visasclean
  
# KNN requires numeric predictors, it seems. So selecting for only factors and numbers: 
str(visasclean)
visasclean <- visasclean[,sapply(visasclean,is.factor) | sapply(visasclean,is.numeric)]

#Standardizing
visasclean %>% mutate(annual_wage=scale(annual_wage),
                      gdp_per_cap=scale(gdp_per_cap),
                      migration_per_cap=scale(migration_per_cap),
                      net_migration=scale(net_migration),
                      pop_muslim=scale(pop_muslim),
                      population=scale(population),
                      pw_amount=scale(pw_amount),
                      unemployment=scale(unemployment),
                      wage_offer_from=scale(wage_offer_from)
) -> visasclean

# Removing NAS, because KNN can't handle them. First, 
# Where are the NAs?
sapply(visasclean, function(x) sum(is.na(x)))
# Then: Select for complete rows
visasclean = visasclean[complete.cases(visasclean), ]
# Leaves us with 514,522 observations. 

# Splitting into test and train 
require(caTools)
set.seed(1) 
sample = sample.split(visasclean$case_status_quant, SplitRatio = .50)
#Splitting into 0.50 becuase test and train have to be the same in KNN (I think)
train = subset(visasclean, sample == TRUE)
train.Y <- train %>% select(case_status_quant)
train.X = train %>% select(-case_status_quant, -case_status_new, -case_status)
test  = subset(visasclean, sample == FALSE)
test.Y <- test %>% select(case_status_quant) 
test.X = test %>% select(-case_status_quant, -case_status_new, -case_status)

# KNN - Code doesn't work. 
# knn.pred=knn(train=train.X,test=test.X,cl=train.Y,k=1)
# Doesn't work; need only numeric variables
# train.Y.new <- train.Y[,1, drop=TRUE]
# knn.pred=knn(train.X,test.X,train.Y.new,k=1)
# Still doesn't work, despite removing all variables that aren't factors or numbers. 
