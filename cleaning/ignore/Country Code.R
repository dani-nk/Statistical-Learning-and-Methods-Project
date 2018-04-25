countries <- read.csv("~/Documents/2017-2018 UT Austin /AEM/Semester Project/4f67b2ca-0887-4ec5-9155-e79b50faf5a8_Data.csv")
View(countries)
df = subset(countries, Series.Code=='NY.GDP.PCAP.CD' | Series.Code=='SL.UEM.TOTL.ZS' | Series.Code=='SM.POP.NETM',select = -c(Country.Code,Series.Name,X2017..YR2017.,X2010..YR2010.,X2015..YR2015.,X2016..YR2016.))
countries.wide <- reshape(df, idvar = "Country.Name", timevar = "Series.Code", direction = "wide")
View(countries.wide)
colnames(countries.wide) <- c("country_of_origin","gpd_per_cap","unemployment","net_migration")
library(dplyr)
countries.wide <- mutate_all(countries.wide, funs(toupper))
population <- read.csv("~/Documents/2017-2018 UT Austin /AEM/Semester Project/99ceaeae-170e-4afd-a285-313b9df5daa4_Data.csv")
population <- subset(population,select = c(Country.Name,X2012..YR2012.))
colnames(population) <- c("country_of_origin","population")
population <- mutate_all(population, funs(toupper))
countries2 <- merge(countries.wide,population,by="country_of_origin")
countries2$net_migration[countries2$net_migration=='..'] <- NA
countries2$gpd_per_cap[countries2$gpd_per_cap=='..'] <- NA
countries2$unemployment[countries2$unemployment=='..'] <- NA
countries2$population[countries2$population=='..'] <- NA

countries2$net_migration <- as.numeric(countries2$net_migration)
countries2$population <- as.numeric(countries2$population)
countries2$gdp_per_cap <- as.numeric(countries2$gdp_per_cap)
countries2$unemployment <- as.numeric(countries2$unemployment)

countries2$migration_per_cap <- 100*(countries2$net_migration / countries2$population)

my.table3 <- merge(my.table, countries2, by.x="Var1", by.y="country_of_origin", all=TRUE)
#Countries that are unmatched include (PLUS ALL THE ONES I FIXED BELOW)
#ANGUILLA
#CZECHOSLOVAKIA (can we recode - but to what?)
#LAOS
#MONTSERRAT
#PALESTINE
#PALESTINIAN TERRITORIES (can we recode?)
#PITCAIRN ISLANDS
#SERBIA AND MONTENEGRO
#ST HELENA
#TAIWAN
#YUGOSLAVIA (no longer exists either...)

countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="BAHAMAS, THE","BAHAMAS")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="BRUNEI DARUSSALAM","BRUNEI")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="CABO VERDE","CAPE VERDE") 
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="CONGO, DEM. REP.","DEMOCRATIC REPUBLIC OF CONGO")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="EGYPT, ARAB REP.","EGYPT")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="GAMBIA, THE","GAMBIA")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="HONG KONG SAR, CHINA","HONG KONG")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="IRAN, ISLAMIC REP.","IRAN")
countries2$country_of_origin <- replace(countries2$country_of_origin, countries2$country_of_origin=="COTE D'IVOIRE","IVORY COAST")
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

visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="BURMA (MYANMAR)","MYANMAR")
visas$country_of_origin <- replace(visas$country_of_origin, visas$country_of_origin=="SOVIET UNION","RUSSIA")

my.table4 <- merge(my.table, countries2, by.x="Var1", by.y="country_of_origin", all=TRUE)

visas2 <- merge(visas,countries2,by="country_of_origin", all.x=TRUE)
View(visas2)

religion <- read.csv("~/Documents/2017-2018 UT Austin /AEM/Semester Project/WRP_national.csv")


