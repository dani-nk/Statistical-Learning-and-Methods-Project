#Read in country data from World Bank
countries <- read.csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/data/4f67b2ca-0887-4ec5-9155-e79b50faf5a8_Data.csv")
View(countries)

#Read in visa data and create table of country frequencies
visas <- read.csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/data/1035PM_4-3-18_2008_2018_PERM.csv")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="BURMA (MYANMAR)","MYANMAR")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="SOVIET UNION","RUSSIA")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="PALESTINE","PALESTINIAN TERRITORIES")
country.freq <- table(visas$country_of_origin)
View(country.freq)

#Drop unnecessary columns and reshape wide
df = subset(countries, Series.Code=='NY.GDP.PCAP.CD' | Series.Code=='SL.UEM.TOTL.ZS' | Series.Code=='SM.POP.NETM',select = -c(Country.Code,Series.Name,X2017..YR2017.,X2010..YR2010.,X2015..YR2015.,X2016..YR2016.))
countries.wide <- reshape(df, idvar = "Country.Name", timevar = "Series.Code", direction = "wide")
View(countries.wide)
colnames(countries.wide) <- c("country_of_origin","gdp_per_cap","unemployment","net_migration")

#Standardize country names to uppercase
library(dplyr)
countries.wide <- mutate_all(countries.wide, funs(toupper))

#Pull in population data and clean
population <- read.csv("~/Documents/2017-2018 UT Austin /AEM/Semester Project/99ceaeae-170e-4afd-a285-313b9df5daa4_Data.csv")
population <- subset(population,select = c(Country.Name,X2012..YR2012.))
colnames(population) <- c("country_of_origin","population")
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
religion <- read.csv("~/Documents/GitHub/Statistical-Learning-and-Methods-Project/data/Muslim Updated.csv")
View(religion)

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
countries2 <- merge(countries2, religion, by="country_of_origin", all=TRUE)
View(countries2)

#Merge into final visas2 dataset
visas2 <- merge(visas,countries2,by="country_of_origin", all.x=TRUE)
View(visas2)



