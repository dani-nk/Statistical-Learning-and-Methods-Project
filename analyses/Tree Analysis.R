visasclean <- read.csv("~/Documents/GitHub/visasclean.csv")

visasclean$decision_year <- as.numeric(substr(as.character(visasclean$decision_date), 1, 4))
visasclean$decision_year <- as.factor(visasclean$decision_year)

#Recode class_of_admission and 

library(tree)
attach(visasclean)
case_status_new <- as.factor(case_status_new)

set.seed(1)
train <- sample(1:nrow(visasclean), 0.7*nrow(visasclean))
visasclean.train <- visasclean[train ,]
visasclean.test <- visasclean[-train ,]

#Try classification tree
tree1 <- tree(case_status_new~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+naics_code_new,visasclean.train)
summary(tree1)
plot(tree1)
text(tree1,pretty=0)
#Everything is classified as certified. In order to get at differences I have to do quant as below

#Run a quantitative tree
tree2 <- tree(case_status_quant~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim+naics_code_new,visasclean.train)
summary(tree2)
plot(tree2)
text(tree2,pretty=0)

tree3 <- tree(case_status_quant~gdp_per_cap+unemployment+pop_muslim,visasclean)
summary(tree3)
plot(tree3)
text(tree3,pretty=0)

cv.tree3 <- cv.tree(tree3)
plot(cv.tree3,cv.tree3$dev,type='b')