library(base)
data<-X1035PM_4_3_18_2008_2018_PERM
data$country_of_origin <- toupper(data$country_of_origin)
data$pw_unit_of_pay <- toupper(pw_unit_of_pay)
table(data$pw_unit_of_pay)
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="HOUR","HR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="WEEK","WK")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="BI-WEEKLY","BI")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="YEAR","YR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="MONTH","MTH")

#check to make sure transformations worked
table(data$pw_unit_of_pay)

#Translate all units of pay into yearly using the following translations: YR = 2000 HR, 50 WK, 25 BI, 12 MTH
data$annual_wage <- ifelse(data$pw_unit_of_pay=="HR",data$pw_amount*2000,data$pw_amount)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="WK",data$pw_amount*50,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="BI",data$pw_amount*25,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="MTH",data$pw_amount*12,data$annual_wage)
View(data)

#exclude values below 9,600 annual wage, which were entered incorrectly in original data as the wrong unit and are not actually annual wage
newdata_no_typo_wages<-subset(data,annual_wage>=9600)
#check
View(newdata_no_typo_wages)
#write to data
X1035PM_4_3_18_2008_2018_PERM<-newdata_no_typo_wages
View(X1035PM_4_3_18_2008_2018_PERM)

