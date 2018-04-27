###Cleaning Script
##Clean wage data to YR units and to remove incorrectly entered observations

library(readr)
visas <- read_csv("visas.csv")

data<-visas
data$country_of_origin <- toupper(data$country_of_origin)
data$pw_unit_of_pay <- toupper(data$pw_unit_of_pay)
data$case_status <- toupper(data$case_status)
data$employer_name <- toupper(data$employer_name)
data$employer_city <- toupper(data$employer_city)
data$employer_state <- toupper(data$employer_state)
data$job_city <- toupper(data$job_city)
data$job_state <- toupper(data$job_state)
data$job_state <- toupper(data$job_state)
data$pw_job_title <- toupper(data$pw_job_title)
data$naics_title <- toupper(data$naics_title)



#View(data)

table(data$pw_unit_of_pay)

#Translate to caps 2-char units
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="HOUR","HR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="WEEK","WK")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="BI-WEEKLY","BI")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="YEAR","YR")
data$pw_unit_of_pay <- replace(data$pw_unit_of_pay,data$pw_unit_of_pay=="MONTH","MTH")

#check to make sure transformations worked as they should
table(data$pw_unit_of_pay)

#Translate all units of pay into yearly using the following translations: YR = 2000 HR, 50 WK, 25 BI, 12 MTH
data$annual_wage <- ifelse(data$pw_unit_of_pay=="HR",data$pw_amount*2000,data$pw_amount)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="WK",data$pw_amount*50,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="BI",data$pw_amount*25,data$annual_wage)
data$annual_wage <- ifelse(data$pw_unit_of_pay=="MTH",data$pw_amount*12,data$annual_wage)

#View(data)

#exclude values below 9,600 annual wage, which were entered incorrectly in original data as the wrong unit and are not actually annual wage
newdata_no_typo_wages<-subset(data,annual_wage>=9600)
#View(newdata_no_typo_wages)
#write to data
visas <- newdata_no_typo_wages
View(visas)

##Append and clean country data
#Read in country data from World Bank
countries <- read.csv("data/4f67b2ca-0887-4ec5-9155-e79b50faf5a8_Data.csv")
#View(countries)
#View(visas)
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="BURMA (MYANMAR)","MYANMAR")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="SOVIET UNION","RUSSIA")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="PALESTINE","PALESTINIAN TERRITORIES")
country.freq <- table(visas$country_of_origin)
#View(country.freq)

#Drop unnecessary columns and reshape wide
countrydata <- subset(countries, Series.Code=='NY.GDP.PCAP.CD' | Series.Code=='SL.UEM.TOTL.ZS' | Series.Code=='SM.POP.NETM',select = -c(Country.Code,Series.Name,X2017..YR2017.,X2010..YR2010.,X2015..YR2015.,X2016..YR2016.))
countries.wide <- reshape(countrydata, idvar = "Country.Name", timevar = "Series.Code", direction = "wide")
#View(countries.wide)
colnames(countries.wide) <- c("country_of_origin","gdp_per_cap","unemployment","net_migration")

#Standardize country names to uppercase
library(dplyr)
countries.wide <- mutate_all(countries.wide, funs(toupper))

#Pull in population data and clean
population <- read.csv("data/99ceaeae-170e-4afd-a285-313b9df5daa4_Data.csv")
colnames(population) <- c("country_of_origin","X","Y","Z","population")
population <- subset(population,select = c(country_of_origin,population))
population <- mutate_all(population, funs(toupper))

#Add population data to create countries2
countries2 <- merge(countries.wide,population,by="country_of_origin")

#Fix NA values and turn to numeric
countries2$net_migration[countries2$net_migration=='..'] <- NA
countries2$gdp_per_cap[countries2$gdp_per_cap=='..'] <- NA
countries2$unemployment[countries2$unemployment=='..'] <- NA
countries2$population[countries2$population=='..'] <- NA

countries2$net_migration <- as.numeric(countries2$net_migration)
countries2$population <- as.numeric(countries2$population)
countries2$gdp_per_cap <- as.numeric(countries2$gdp_per_cap)
countries2$unemployment <- as.numeric(countries2$unemployment)

#Create migration_per_cap variable
countries2$migration_per_cap <- 100*(countries2$net_migration / countries2$population)

#Check which countries  are unmatched
my.table <- merge(country.freq, countries2, by.x="Var1", by.y="country_of_origin", all=TRUE)

#Fix countries that have matches but are denoted differently
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="BAHAMAS, THE","BAHAMAS")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="BRUNEI DARUSSALAM","BRUNEI")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="CABO VERDE","CAPE VERDE") 
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="CONGO, DEM. REP.","DEMOCRATIC REPUBLIC OF CONGO")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="EGYPT, ARAB REP.","EGYPT")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="GAMBIA, THE","GAMBIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="HONG KONG SAR, CHINA","HONG KONG")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="IRAN, ISLAMIC REP.","IRAN")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="COTE D'IVOIRE","IVORY COAST")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="LAO PDR","LAOS")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="KYRGYZ REPUBLIC","KYRGYZSTAN")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="MACAO SAR, CHINA","MACAU")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="MACEDONIA, FYR","MACEDONIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="MICRONESIA, FED. STS.","MICRONESIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="KOREA, DEM. PEOPLE’S REP.","NORTH KOREA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="CONGO, REP.","REPUBLIC OF CONGO")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="RUSSIAN FEDERATION","RUSSIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="SLOVAK REPUBLIC","SLOVAKIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="KOREA, REP.","SOUTH KOREA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="ST. KITTS AND NEVIS","ST KITTS AND NEVIS")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="ST. LUCIA","ST LUCIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="ST. VINCENT AND THE GRENADINES","ST VINCENT")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="SYRIAN ARAB REPUBLIC","SYRIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="UNITED STATES","UNITED STATES OF AMERICA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="VENEZUELA, RB","VENEZUELA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="YEMEN, REP.","YEMEN")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="WEST BANK AND GAZA","PALESTINIAN TERRITORIES")

#Check matches again
my.table2 <- merge(country.freq, countries2, by.x="Var1", by.y="country_of_origin", all=TRUE)

#Countries that are still unmatched include
#ANGUILLA (British territory in Carribbean)
#CZECHOSLOVAKIA (No longer exists)
#MONTSERRAT (British territory in Carribbean)
#PITCAIRN ISLANDS (British territory in South Pacific)
#SERBIA AND MONTENEGRO (No longer exists)
#ST HELENA (British territory in South Atlantic)
#TAIWAN (Not recognized by World Bank)
#YUGOSLAVIA (No longer exists)

#Add religion
religion <- read.csv("data/Muslim Updated.csv")
#View(religion)

#Clean up religion file
religion <- subset(religion,select = c(Country..region,X..of.pop.that.is.muslim))
colnames(religion) <- c("country_of_origin","pop_muslim")
religion <- mutate_all(religion, funs(toupper))
religion$country_of_origin <- trimws(religion$country_of_origin, which = c("both", "left", "right"))
religion$pop_muslim <- as.numeric(religion$pop_muslim)
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="BOSNIA-HERZEGOVINA","BOSNIA AND HERZEGOVINA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="BURMA (MYANMAR)","MYANMAR")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="FAEROE ISLANDS","FAROE ISLANDS")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="FEDERATED STATES OF MICRONESIA","MICRONESIA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="GUINEA BISSAU","GUINEA-BISSAU")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="REPUBLIC OF MACEDONIA","MACEDONIA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="ST. HELENA","ST HELENA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="ST. KITTS AND NEVIS","ST KITTS AND NEVIS")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="ST. LUCIA","ST LUCIA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="ST. VINCENT AND THE GRENADINES","ST VINCENT")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="U.S. VIRGIN ISLANDS","VIRGIN ISLANDS (U.S.)")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="UNITED STATES","UNITED STATES OF AMERICA")
religion$country_of_origin <- replace(religion$country_of_origin, religion$country_of_origin=="CONGO","DEMOCRATIC REPUBLIC OF CONGO")

#Merge religion file into country file
countries3 <- merge(countries2, religion, by="country_of_origin", all=TRUE)
#View(countries3)

#Merge into final visas2 dataset
visas2 <- merge(visas,countries3,by="country_of_origin", all.x=TRUE)
#View(visas2)
visas<- visas2

##Reduce outcome variables to two classes
visas$naics_code_new <- as.numeric(substr(as.character(visas$naics_code), 1, 2))
visas$case_status_new <- replace(visas$case_status, visas$case_status=="CERTIFIED-EXPIRED","CERTIFIED")
no_withdrawn<-subset(visas, case_status_new=="CERTIFIED" | case_status_new=="DENIED")
no_withdrawn$case_status_quant <- ifelse(no_withdrawn$case_status_new=="CERTIFIED",1,0)
visas <- no_withdrawn
visas$naics_code_new <- as.factor(visas$naics_code_new)

##Fix state names
#Cleaning state data 
visas$employer_state[visas$employer_state=="WYOMING"] = "WY"
visas$employer_state[visas$employer_state=="CALIFORNIA"] = "CA"
visas$employer_state[visas$employer_state=="NEW JERSEY"] = "NJ"
visas$employer_state[visas$employer_state=="NEW YORK"] = "NY"
visas$employer_state[visas$employer_state=="WASHINGTON"] = "WA"
visas$employer_state[visas$employer_state=="ILLINOIS"] = "IL"
visas$employer_state[visas$employer_state=="MASSACHUSETTS"] = "MA"
visas$employer_state[visas$employer_state=="PENNSYLVANIA"] = "PA"
visas$employer_state[visas$employer_state=="VIRGINIA"] = "VA"
visas$employer_state[visas$employer_state=="MICHIGAN"] = "MI"
visas$employer_state[visas$employer_state=="FLORIDA"] = "FL"
visas$employer_state[visas$employer_state=="GEORGIA"] = "GA"
visas$employer_state[visas$employer_state=="OHIO"] = "OH"
visas$employer_state[visas$employer_state=="MARYLAND"] = "MD"
visas$employer_state[visas$employer_state=="NORTH CAROLINA"] = "NC"
visas$employer_state[visas$employer_state=="CONNECTICUT"] = "CT"
visas$employer_state[visas$employer_state=="ARIZONA"] = "AZ"
visas$employer_state[visas$employer_state=="WISCONSIN"] = "WI"
visas$employer_state[visas$employer_state=="MISSOURI"] = "MO"
visas$employer_state[visas$employer_state=="INDIANA"] = "IN"
visas$employer_state[visas$employer_state=="MINNESOTA"] = "MN"
visas$employer_state[visas$employer_state=="COLORADO"] = "CO"
visas$employer_state[visas$employer_state=="TENNESSEE"] = "TN"
visas$employer_state[visas$employer_state=="ALABAMA"] = "AL"
visas$employer_state[visas$employer_state=="DISTRICT OF COLUMBIA"] = "DC"
visas$employer_state[visas$employer_state=="KANSAS"] = "KS"
visas$employer_state[visas$employer_state=="KENTUCKY"] = "KY"
visas$employer_state[visas$employer_state=="IOWA"] = "IA"
visas$employer_state[visas$employer_state=="DELAWARE"] = "DE"
visas$employer_state[visas$employer_state=="ARKANSAS"] = "AR"
visas$employer_state[visas$employer_state=="ORGEGON"] = "OR"
visas$employer_state[visas$employer_state=="NEBRASKA"] = "NE"
visas$employer_state[visas$employer_state=="LOUISIANA"] = "LA"
visas$employer_state[visas$employer_state=="SOUTH CAROLINA"] = "SC"
visas$employer_state[visas$employer_state=="OKLAHOMA"] = "OK"
visas$employer_state[visas$employer_state=="NEW HAMPSHIRE"] = "NH"
visas$employer_state[visas$employer_state=="IDAHO"] = "ID"
visas$employer_state[visas$employer_state=="RHODE ISLAND"] = "RI"
visas$employer_state[visas$employer_state=="NEVADA"] = "NV"
visas$employer_state[visas$employer_state=="NEW MEXICO"] = "NM"
visas$employer_state[visas$employer_state=="MISSISSIPPI"] = "MS"
visas$employer_state[visas$employer_state=="VERMONT"] = "VT"
visas$employer_state[visas$employer_state=="SOUTH DAKOTA"] = "SD"
visas$employer_state[visas$employer_state=="WEST VIRGINIA"] = "WV"
visas$employer_state[visas$employer_state=="MAINE"] = "ME"
visas$employer_state[visas$employer_state=="GUAM"] = "GU"
visas$employer_state[visas$employer_state=="HAWAII"] = "HI"
visas$employer_state[visas$employer_state=="MONTANA"] = "MT"
visas$employer_state[visas$employer_state=="PUERTO RICO"] = "PR"
visas$employer_state[visas$employer_state=="TEXAS"] = "TX"
visas$employer_state[visas$employer_state=="OREGON"] = "OR"
visas$employer_state[visas$employer_state=="UTAH"] = "UT"
visas$employer_state[visas$employer_state=="NORTH DAKOTA"] = "ND"
visas$employer_state[visas$employer_state=="ALASKA"] = "AK"
visas$employer_state[visas$employer_state=="PUERTO RICO"] = "PR"
visas$employer_state[visas$employer_state=="VIRGIN ISLANDS"] = "VI"

visas$job_state[visas$job_state=="WYOMING"] = "WY"
visas$job_state[visas$job_state=="CALIFORNIA"] = "CA"
visas$job_state[visas$job_state=="NEW JERSEY"] = "NJ"
visas$job_state[visas$job_state=="NEW YORK"] = "NY"
visas$job_state[visas$job_state=="WASHINGTON"] = "WA"
visas$job_state[visas$job_state=="ILLINOIS"] = "IL"
visas$job_state[visas$job_state=="MASSACHUSETTS"] = "MA"
visas$job_state[visas$job_state=="PENNSYLVANIA"] = "PA"
visas$job_state[visas$job_state=="VIRGINIA"] = "VA"
visas$job_state[visas$job_state=="MICHIGAN"] = "MI"
visas$job_state[visas$job_state=="FLORIDA"] = "FL"
visas$job_state[visas$job_state=="GEORGIA"] = "GA"
visas$job_state[visas$job_state=="OHIO"] = "OH"
visas$job_state[visas$job_state=="MARYLAND"] = "MD"
visas$job_state[visas$job_state=="NORTH CAROLINA"] = "NC"
visas$job_state[visas$job_state=="CONNECTICUT"] = "CT"
visas$job_state[visas$job_state=="ARIZONA"] = "AZ"
visas$job_state[visas$job_state=="WISCONSIN"] = "WI"
visas$job_state[visas$job_state=="MISSOURI"] = "MO"
visas$job_state[visas$job_state=="INDIANA"] = "IN"
visas$job_state[visas$job_state=="MINNESOTA"] = "MN"
visas$job_state[visas$job_state=="COLORADO"] = "CO"
visas$job_state[visas$job_state=="TENNESSEE"] = "TN"
visas$job_state[visas$job_state=="ALABAMA"] = "AL"
visas$job_state[visas$job_state=="DISTRICT OF COLUMBIA"] = "DC"
visas$job_state[visas$job_state=="KANSAS"] = "KS"
visas$job_state[visas$job_state=="KENTUCKY"] = "KY"
visas$job_state[visas$job_state=="IOWA"] = "IA"
visas$job_state[visas$job_state=="DELAWARE"] = "DE"
visas$job_state[visas$job_state=="ARKANSAS"] = "AR"
visas$job_state[visas$job_state=="ORGEGON"] = "OR"
visas$job_state[visas$job_state=="NEBRASKA"] = "NE"
visas$job_state[visas$job_state=="LOUISIANA"] = "LA"
visas$job_state[visas$job_state=="SOUTH CAROLINA"] = "SC"
visas$job_state[visas$job_state=="OKLAHOMA"] = "OK"
visas$job_state[visas$job_state=="NEW HAMPSHIRE"] = "NH"
visas$job_state[visas$job_state=="IDAHO"] = "ID"
visas$job_state[visas$job_state=="RHODE ISLAND"] = "RI"
visas$job_state[visas$job_state=="NEVADA"] = "NV"
visas$job_state[visas$job_state=="NEW MEXICO"] = "NM"
visas$job_state[visas$job_state=="MISSISSIPPI"] = "MS"
visas$job_state[visas$job_state=="VERMONT"] = "VT"
visas$job_state[visas$job_state=="SOUTH DAKOTA"] = "SD"
visas$job_state[visas$job_state=="WEST VIRGINIA"] = "WV"
visas$job_state[visas$job_state=="MAINE"] = "ME"
visas$job_state[visas$job_state=="GUAM"] = "GU"
visas$job_state[visas$job_state=="HAWAII"] = "HI"
visas$job_state[visas$job_state=="MONTANA"] = "MT"
visas$job_state[visas$job_state=="PUERTO RICO"] = "PR"
visas$job_state[visas$job_state=="TEXAS"] = "TX"
visas$job_state[visas$job_state=="OREGON"] = "OR"
visas$job_state[visas$job_state=="UTAH"] = "UT"
visas$job_state[visas$job_state=="NORTH DAKOTA"] = "ND"
visas$job_state[visas$job_state=="ALASKA"] = "AK"
visas$job_state[visas$job_state=="PUERTO RICO"] = "PR"
visas$job_state[visas$job_state=="VIRGIN ISLANDS"] = "VI"
table(visas$employer_state)

visasclean<-visas

View(visasclean)

##Condense country_of_origin and class_of_admission
visasclean %>% mutate(country_new = 
                        ifelse(country_of_origin == "INDIA", "INDIA", 
                               ifelse(country_of_origin == "CHINA", "CHINA", 
                                      ifelse(country_of_origin == "SOUTH KOREA", "SOUTH KOREA", 
                                             ifelse(country_of_origin == "CANADA", "CANADA", 
                                                    ifelse(country_of_origin == "MEXICO", "MEXICO", 
                                                           ifelse(country_of_origin == "PHILIPPINES", "PHILIPPINES", 
                                                                  ifelse(country_of_origin == "UNITED KINGDOM", "UNITED KINGDOM", 
                                                                         ifelse(country_of_origin == "TAIWAN", "TAIWAN", 
                                                                                ifelse(country_of_origin == "PAKISTAN", "PAKISTAN", 
                                                                                       ifelse(country_of_origin == "BRAZIL", "BRAZIL",          
                                                                                              "OTHER"))))))))))) -> visasclean
topten_class_of_admission <-table(visasclean$class_of_admission)
#View(topten_class_of_admission)
visasclean %>% mutate(class_new = 
                        ifelse(visasclean$class_of_admission == "H-1B", "H-1B", 
                               ifelse(visasclean$class_of_admission == "L-1", "L-1", 
                                      ifelse(visasclean$class_of_admission == "F-1", "F-1", 
                                             ifelse(visasclean$class_of_admission == "B-2", "B-2", 
                                                    ifelse(visasclean$class_of_admission == "Not in USA", "Not in USA", 
                                                           ifelse(visasclean$class_of_admission == "EWI", "EWI", 
                                                                  ifelse(visasclean$class_of_admission == "E-2", "E-2", 
                                                                         ifelse(visasclean$class_of_admission == "TN", "TN", 
                                                                                ifelse(visasclean$class_of_admission == "Parolee", "Parolee", 
                                                                                       ifelse(visasclean$class_of_admission == "H-1B1", "H-1B1",          
                                                                                              "OTHER"))))))))))) -> visasclean
visasclean %>% mutate(job_state_new = 
                        ifelse(visasclean$job_state == "CA", "CA", 
                               ifelse(visasclean$job_state == "TX", "TX", 
                                      ifelse(visasclean$job_state == "NJ", "NJ", 
                                             ifelse(visasclean$job_state == "NY", "NY", 
                                                    ifelse(visasclean$job_state == "WA", "WA", 
                                                           ifelse(visasclean$job_state == "IL", "IL", 
                                                                  ifelse(visasclean$job_state == "FL", "FL", 
                                                                         ifelse(visasclean$job_state == "VA", "VA", 
                                                                                ifelse(visasclean$job_state == "MA", "MA", 
                                                                                       ifelse(visasclean$job_state == "GA", "GA", ifelse(visasclean$job_state == "MI", "MI", ifelse(visasclean$job_state == "PA", "PA", ifelse(visasclean$job_state == "NC", "NC", ifelse(visasclean$job_state == "MD", "MD", ifelse(visasclean$job_state == "OH", "OH",         
                                                                                                                                                                                                                                                                                                                     "OTHER")))))))))))))))) -> visasclean
## Removing NAS
visasclean %>% select(-wage_offer_to) -> visasclean
sapply(visasclean, function(x) sum(is.na(x)))

## Then: Select for complete rows

visasclean = visasclean[complete.cases(visasclean), ]

dim(visasclean)
## Leaves us with 514,522 observations.

# Add column for year
visasclean$decision_year_numeric <- as.numeric(substr(as.character(visasclean$decision_date), 1, 4))
visasclean$decision_year <- as.factor(visasclean$decision_year_numeric)

#write
write_csv(visas, "~/Documents/GitHub/visasclean.csv")
#View(visasclean)
