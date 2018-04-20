visasclean <- read.csv("~/Documents/GitHub/visasclean.csv")

library(tree)
attach(visasclean)
tree1 <- tree(case_status_quant~annual_wage+gdp_per_cap+unemployment+migration_per_cap+pop_muslim,visasclean)
summary(tree1)
plot(tree1)
text(tree1,pretty=0)

tree2 <- tree(case_status_quant~gdp_per_cap+unemployment+migration_per_cap+pop_muslim,visasclean)
summary(tree2)
plot(tree2)
text(tree2,pretty=0)

tree3 <- tree(case_status_quant~gdp_per_cap+unemployment+pop_muslim,visasclean)
summary(tree3)
plot(tree3)
text(tree3,pretty=0)

cv.tree3 <- cv.tree(tree3)
plot(cv.tree3,cv.tree3$dev,type='b')