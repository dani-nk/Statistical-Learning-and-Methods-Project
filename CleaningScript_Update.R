###Cleaning Script
##Clean wage data to YR units and to remove incorrectly entered observations
#Import dataset from GitHub Files
data<-read.csv("data/1035PM_4-3-18_2008_2018_PERM.csv")
data$country_of_origin <- toupper(data$country_of_origin)
data$pw_unit_of_pay <- toupper(data$pw_unit_of_pay)
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
X1035PM_4_3_18_2008_2018_PERM <- newdata_no_typo_wages
#View(X1035PM_4_3_18_2008_2018_PERM)

##Append and clean country data
#Read in country data from World Bank
countries <- read.csv("data/4f67b2ca-0887-4ec5-9155-e79b50faf5a8_Data.csv")
#View(countries)
#Read in visa data and create table of country frequencies
visas <- X1035PM_4_3_18_2008_2018_PERM
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
population <- read_csv("data/99ceaeae-170e-4afd-a285-313b9df5daa4_Data.csv")
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
X1035PM_4_3_18_2008_2018_PERM <- visas2

##Reduce outcome variables to two classes
X1035PM_4_3_18_2008_2018_PERM$naics_code_new <- as.numeric(substr(as.character(X1035PM_4_3_18_2008_2018_PERM$naics_code), 1, 2))
X1035PM_4_3_18_2008_2018_PERM$case_status_new <- replace(X1035PM_4_3_18_2008_2018_PERM$case_status, X1035PM_4_3_18_2008_2018_PERM$case_status=="CERTIFIED-EXPIRED","CERTIFIED")
no_withdrawn<-subset(X1035PM_4_3_18_2008_2018_PERM, case_status_new=="CERTIFIED" | case_status_new=="DENIED")
no_withdrawn$case_status_quant <- ifelse(no_withdrawn$case_status_new=="CERTIFIED",1,0)
X1035PM_4_3_18_2008_2018_PERM <- no_withdrawn

##Fix state names
#Cleaning state data 
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="WYOMING"] = "WY"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="CALIFORNIA"] = "CA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEW JERSEY"] = "NJ"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEW YORK"] = "NY"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="WASHINGTON"] = "WA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ILLINOIS"] = "IL"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MASSACHUSETTS"] = "MA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="PENNSYLVANIA"] = "PA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="VIRGINIA"] = "VA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MICHIGAN"] = "MI"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="FLORIDA"] = "FL"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="GEORGIA"] = "GA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="OHIO"] = "OH"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MARYLAND"] = "MD"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NORTH CAROLINA"] = "NC"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="CONNECTICUT"] = "CT"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ARIZONA"] = "AZ"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="WISCONSIN"] = "WI"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MISSOURI"] = "MO"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="INDIANA"] = "IN"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MINNESOTA"] = "MN"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="COLORADO"] = "CO"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="TENNESSEE"] = "TN"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ALABAMA"] = "AL"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="DISTRICT OF COLUMBIA"] = "DC"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="KANSAS"] = "KS"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="KENTUCKY"] = "KY"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="IOWA"] = "IA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="DELAWARE"] = "DE"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ARKANSAS"] = "AR"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ORGEGON"] = "OR"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEBRASKA"] = "NE"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="LOUISIANA"] = "LA"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="SOUTH CAROLINA"] = "SC"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="OKLAHOMA"] = "OK"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEW HAMPSHIRE"] = "NH"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="IDAHO"] = "ID"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="RHODE ISLAND"] = "RI"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEVADA"] = "NV"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NEW MEXICO"] = "NM"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MISSISSIPPI"] = "MS"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="VERMONT"] = "VT"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="SOUTH DAKOTA"] = "SD"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="WEST VIRGINIA"] = "WV"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MAINE"] = "ME"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="GUAM"] = "GU"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="HAWAII"] = "HI"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="MONTANA"] = "MT"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="PUERTO RICO"] = "PR"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="TEXAS"] = "TX"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="OREGON"] = "OR"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="UTAH"] = "UT"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="NORTH DAKOTA"] = "ND"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="ALASKA"] = "AK"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="PUERTO RICO"] = "PR"
X1035PM_4_3_18_2008_2018_PERM$employer_state[X1035PM_4_3_18_2008_2018_PERM$employer_state=="VIRGIN ISLANDS"] = "VI"

X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="WYOMING"] = "WY"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="CALIFORNIA"] = "CA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEW JERSEY"] = "NJ"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEW YORK"] = "NY"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="WASHINGTON"] = "WA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ILLINOIS"] = "IL"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MASSACHUSETTS"] = "MA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="PENNSYLVANIA"] = "PA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="VIRGINIA"] = "VA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MICHIGAN"] = "MI"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="FLORIDA"] = "FL"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="GEORGIA"] = "GA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="OHIO"] = "OH"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MARYLAND"] = "MD"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NORTH CAROLINA"] = "NC"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="CONNECTICUT"] = "CT"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ARIZONA"] = "AZ"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="WISCONSIN"] = "WI"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MISSOURI"] = "MO"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="INDIANA"] = "IN"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MINNESOTA"] = "MN"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="COLORADO"] = "CO"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="TENNESSEE"] = "TN"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ALABAMA"] = "AL"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="DISTRICT OF COLUMBIA"] = "DC"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="KANSAS"] = "KS"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="KENTUCKY"] = "KY"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="IOWA"] = "IA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="DELAWARE"] = "DE"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ARKANSAS"] = "AR"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ORGEGON"] = "OR"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEBRASKA"] = "NE"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="LOUISIANA"] = "LA"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="SOUTH CAROLINA"] = "SC"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="OKLAHOMA"] = "OK"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEW HAMPSHIRE"] = "NH"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="IDAHO"] = "ID"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="RHODE ISLAND"] = "RI"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEVADA"] = "NV"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NEW MEXICO"] = "NM"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MISSISSIPPI"] = "MS"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="VERMONT"] = "VT"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="SOUTH DAKOTA"] = "SD"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="WEST VIRGINIA"] = "WV"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MAINE"] = "ME"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="GUAM"] = "GU"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="HAWAII"] = "HI"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="MONTANA"] = "MT"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="PUERTO RICO"] = "PR"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="TEXAS"] = "TX"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="OREGON"] = "OR"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="UTAH"] = "UT"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="NORTH DAKOTA"] = "ND"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="ALASKA"] = "AK"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="PUERTO RICO"] = "PR"
X1035PM_4_3_18_2008_2018_PERM$job_state[X1035PM_4_3_18_2008_2018_PERM$job_state=="VIRGIN ISLANDS"] = "VI"
table(X1035PM_4_3_18_2008_2018_PERM$employer_state)
View(X1035PM_4_3_18_2008_2018_PERM)
