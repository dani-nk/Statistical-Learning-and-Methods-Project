data<-X1035PM_4_3_18_2008_2018_PERM
toupper(data$country_of_origin)
toupper(data$pw_unit_of_pay)
table(data$pw_unit_of_pay)

#Translate all units of pay into yearly using the following translations: YR = 2000 HR, 50 WK, 25 BI, 12 MTH
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="HOUR","HR")
data$annual_wage <- ifelse(data$pw_unit_of_pay=="HR",data$pw_amount*2000,data$pw_amount)
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="WEEK","WK")
data$annual_wage <- ifelse(data$pw_unit_of_pay=="WK",data$pw_amount*50,data$pw_amount)
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="BI-WEEKLY","BI")
data$annual_wage <- ifelse(data$pw_unit_of_pay=="BI",data$pw_amount*25,data$pw_amount)
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="YEAR","YR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="MONTH","MTH")
data$annual_wage <- ifelse(data$pw_unit_of_pay=="MTH",data$pw_amount*12,data$pw_amount)

#clean up extra columns
data <-subset(data,select=-c(newwage, annualwage))
attach(data)
#error -- not all are actually converting. Code was incorrectly overwriting new annual_wage column with old pw_amount column for != values).

#Try again
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="HOUR","HR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="WEEK","WK")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="BI-WEEKLY","BI")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="YEAR","YR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="MONTH","MTH")

data$annual_wage <- ifelse(data$pw_unit_of_pay=="HR",data$pw_amount*2000,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="WK",data$pw_amount*50,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="BI",data$pw_amount*25,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="MTH",data$pw_amount*12,data$annual_wage)
View(data)

newdata_no_typo_wages<-subset(data,annual_wage>=9600)
View(newdata_no_typo_wages)
X1035PM_4_3_18_2008_2018_PERM<-newdata_no_typo_wages
