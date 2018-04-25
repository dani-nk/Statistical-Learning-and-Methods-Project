# Attempting, failing to follow pp.155 in ISL. 
library(tidyverse) # for everything 
library(class) # for KNN 
visas <- read_csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/visasclean.csv")
# attach(visas) # What does this do? 

# Changing data types
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
         case_status_quant = as.numeric(case_status_quant),
         country_of_origin = as.factor(country_of_origin)) -> visas

# Removing wage_offer_to because of the number of NAs. 
visas %>% select(-wage_offer_to) -> visas
  
# KNN requires numeric predictors, it seems. So selecting for only factors and numbers: 
str(visas)
visas <- visas[,sapply(visas,is.factor) | sapply(visas,is.numeric)]

#Standardizing
visas %>% mutate(annual_wage=scale(annual_wage),
                      gdp_per_cap=scale(gdp_per_cap),
                      migration_per_cap=scale(migration_per_cap),
                      net_migration=scale(net_migration),
                      pop_muslim=scale(pop_muslim),
                      population=scale(population),
                      pw_amount=scale(pw_amount),
                      unemployment=scale(unemployment),
                      wage_offer_from=scale(wage_offer_from)
) -> visas

# Removing NAS, because KNN can't handle them. First, 
# Where are the NAs?
sapply(visas, function(x) sum(is.na(x)))
# Then: Select for complete rows
visas = visas[complete.cases(visas), ]
# Leaves us with 514,522 observations. 

# Splitting into test and train 
require(caTools)
set.seed(1) 
sample = sample.split(visas$case_status_quant, SplitRatio = .50)
#Splitting into 0.50 becuase test and train have to be the same in KNN (I think)
# train = subset(visas, sample == TRUE)
# train.Y <- train %>% select(case_status_quant)
# train.X = train %>% select(-case_status_quant, -case_status_new, -case_status)
# test  = subset(visas, sample == FALSE)
# test.Y <- test %>% select(case_status_quant) 
# test.X = test %>% select(-case_status_quant, -case_status_new, -case_status)

# KNN #fail - Code doesn't work. 
# knn.pred=knn(train=train.X,test=test.X,cl=train.Y,k=1)
# Doesn't work; need only numeric variables
# train.Y.new <- train.Y[,1, drop=TRUE]
# knn.pred=knn(train.X,test.X,train.Y.new,k=1)
# Still doesn't work, despite removing all variables that aren't factors or numbers. 

# KNN fail #2 
visasnumeric <- visas[,sapply(visas,is.numeric)]
str(visasnumeric)
set.seed(1) 
sample = sample.split(visas$case_status_quant, SplitRatio = .50)
#Splitting into 0.50 becuase test and train have to be the same in KNN (I think)
train.n = subset(visasnumeric, sample == TRUE)
train.Y.n <- train.n %>% select(case_status_quant)
train.X.n = train.n %>% select(-case_status_quant)
test.n  = subset(visasnumeric, sample == FALSE)
test.Y.n <- test.n %>% select(case_status_quant) 
test.X.n = test.n %>% select(-case_status_quant)
# knn.pred.n=knn(train=train.X.n,test=test.X.n,cl=train.Y.n,k=1)
train.Y.n.new <- train.Y.n[,1, drop=TRUE]
# knn.pred=knn(train.X.n,test.X.n,train.Y.n.new,k=1)
# knn.pred=knn(train.X.n,test.X.n,train.Y.n.new,k=1, use.all = FALSE)
# knn.pred=knn(train.X.n,test.X.n,train.Y.n.new,k=101, use.all = FALSE)
