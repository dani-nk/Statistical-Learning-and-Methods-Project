# KNN 
# p.155
library(tidyverse)
visasclean <- read_csv("~/Documents/GitHub/visasclean.csv")
dim(visasclean)
attach(visasclean)
visasclean %>% 
  mutate(naics_code = as.factor(naics_code),
         naics_code_new = as.factor(naics_code_new),
         pw_level = as.factor(pw_level),
         case_status = as.factor(case_status),
         case_status_new = as.factor(case_status_new),
         job_state = as.factor(job_state),
         employer_state = as.factor(employer_state),
         pw_unit_of_pay = as.factor(pw_unit_of_pay),
         pw_soc_code = as.factor(pw_soc_code),
         country_of_origin = as.factor(country_of_origin)
         ) -> visasclean

# visasclean2 <- visasclean %>% mutate(scale(vars=c(15,17:18,21:27))

# visasclean2 <- as.data.frame(scale(visasclean[,c(15,17:18,21:27)]))

