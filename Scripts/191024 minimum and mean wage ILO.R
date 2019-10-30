# Minimum and mean wage from ILO
# Jan Mares
# 24/10/2019

# Libraries
library(data.table)
library(countrycode)
library(Rilostat)


# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# both indicators in PPP2011 USD

data_ilo_minwage <- data.table(get_ilostat("EAR_4MNP_NOC_NB_A"))
data_ilo_meanwage <- data.table(get_ilostat("EAR_4MTH_SEX_ECO_CUR_NB_A"))

#
#

## minimum wage

# filter required columns
data_minwage <- data_ilo_minwage[, .(ref_area, indicator, time, obs_value)]
setnames(data_minwage, c("iso3c","indicator","year","value"))

# data_minwage[, country := countrycode(iso3c, "iso3c", "country.name")] # test iso3 codes
View(data_ilo_minwage)
# replace the indicator value by simpler one
data_minwage[, indicator := "minwage"]
head(data_minwage)

#
#

## mean wage

# filter values in PPP2011 dollars, totals (for both genders)
type <- c("ECO_AGGREGATE_TOTAL", "ECO_AGGREGATE_AGR","ECO_AGGREGATE_MAN",
	 	  "ECO_AGGREGATE_CON","ECO_AGGREGATE_MEL","ECO_AGGREGATE_MKT",
	 	  "ECO_AGGREGATE_PUB")
 
data_meanwage <- data_ilo_meanwage[sex == "SEX_T" & classif1 %in% type & classif2 == "CUR_TYPE_PPP2011",]

# required columns
data_meanwage <- data_meanwage[classif1 == "ECO_AGGREGATE_TOTAL", .(ref_area, source, indicator, type = classif1, year = time,
							       value = obs_value)]

#
#

# selecting the sources for series where we have multiple values
# this is also to remove the duplicates, although not completely as we leave some to
# link the series

# armenia
data_meanwage <- data_meanwage[!(ref_area == "ARM" & !(source %in% c("BA:867","DA:177"))),]

# austria
data_meanwage <- data_meanwage[!(ref_area == "AUT" & !(source %in% c("FA:883","FX:272"))),]

# bulgaria 
data_meanwage <- data_meanwage[!(ref_area == "BGR" & !(source %in% c("CA:914","DA:388"))),]

# belarus
data_meanwage <- data_meanwage[!(ref_area == "BLR" & !(source %in% c("DA:931"))),]

# bolivia
data_meanwage <- data_meanwage[!(ref_area == "BOL" & !(source %in% c("BB:73"))),]

# brazil
data_meanwage <- data_meanwage[!(ref_area == "BRA" & !(source %in% c("BX:356", "BX:6355"))),]

# canada
data_meanwage <- data_meanwage[!(ref_area == "CAN" & !(source %in% c("BA:147"))),]

# chile
data_meanwage <- data_meanwage[!(ref_area == "CHL" & !(source %in% c("DA:134","BX:138"))),]

# costa rica
data_meanwage <- data_meanwage[!(ref_area == "CRI" & !(source %in% c("BX:142"))),]

# cyprus
data_meanwage <- data_meanwage[!(ref_area == "CYP" & !(source %in% c("FA:38"))),]

# czech republic
data_meanwage <- data_meanwage[!(ref_area == "CZE" & !(source %in% c("DA:1050","DA:293"))),]

# germany
data_meanwage <- data_meanwage[!(ref_area == "DEU" & !(source %in% c("BB:304"))),]

# egypt
data_meanwage <- data_meanwage[!(ref_area == "EGY" & !(source %in% c("BA:582","CA:1162"))),]

# spain
data_meanwage <- data_meanwage[!(ref_area == "ESP" & !(source %in% c("DA:248","FA:1131"))),]

# greece
data_meanwage <- data_meanwage[!(ref_area == "GRC" & !(source %in% c("BA:317"))),]

# guatemala
data_meanwage <- data_meanwage[!(ref_area == "GTM" & !(source %in% c("FA:1203","BA:660"))),]

# hungary
data_meanwage <- data_meanwage[!(ref_area == "HUN" & !(source %in% c("CA:1237","DA:3089"))),]

# ireland
data_meanwage <- data_meanwage[!(ref_area == "IRL" & !(source %in% c("DA:410"))),]

# cambodia
data_meanwage <- data_meanwage[!(ref_area == "KHM" & !(source %in% c("BB:3064","BA:1321"))),]

# lithuania
data_meanwage <- data_meanwage[!(ref_area == "LTU" & !(source %in% c("DA:12956","EA:6396"))),]

# luxembourg
data_meanwage <- data_meanwage[!(ref_area == "LUX" & !(source %in% c("FA:189"))),]

# magagascar
data_meanwage <- data_meanwage[!(ref_area == "MDG" & !(source %in% c("BA:7423"))),]

# mexico
data_meanwage <- data_meanwage[!(ref_area == "MEX" & !(source %in% c("BA:463"))),]

# macedonia
data_meanwage <- data_meanwage[!(ref_area == "MKD" & !(source %in% c("DA:3100","DA:2601"))),]

# mauritius
data_meanwage <- data_meanwage[!(ref_area == "MUS" & !(source %in% c("BA:82","DA:1442"))),]

# malaysia
data_meanwage <- data_meanwage[!(ref_area == "MYS" & !(source %in% c("DA:7341"))),]

# norway
data_meanwage <- data_meanwage[!(ref_area == "NOR" & !(source %in% c("DA:287"))),]

# peru
data_meanwage <- data_meanwage[!(ref_area == "PER" & !(source %in% c("BX:375"))),]

# philipines
data_meanwage <- data_meanwage[!(ref_area == "PHL" & !(source %in% c("BA:629","DA:1539"))),]

# poland
data_meanwage <- data_meanwage[!(ref_area == "POL" & !(source %in% c("DA:428"))),]

# puerto rico
data_meanwage <- data_meanwage[!(ref_area == "PRT" & !(source %in% c("BA:199"))),]

# singapur
data_meanwage <- data_meanwage[!(ref_area == "SGP" & !(source %in% c("BA:5873","FA:5921"))),]

# serbia
data_meanwage <- data_meanwage[!(ref_area == "SRB" & !(source %in% c("BA:467","DA:3105"))),]

# slovakia
data_meanwage <- data_meanwage[!(ref_area == "SVK" & !(source %in% c("DA:399"))),]

# slovenia
data_meanwage <- data_meanwage[!(ref_area == "SVN" & !(source %in% c("DA:5911","FX433"))),]

# sweden
data_meanwage <- data_meanwage[!(ref_area == "SWE" & !(source %in% c("DA:3477","DA:1665"))),]

# thailand
data_meanwage <- data_meanwage[!(ref_area == "THA" & !(source %in% c("BA:741"))),]

# tajikistan
data_meanwage <- data_meanwage[!(ref_area == "TJK" & !(source %in% c("CA:1705"))),]

# tanzania
data_meanwage <- data_meanwage[!(ref_area == "TZA" & !(source %in% c("BA:1743","DA:2543"))),]

# south africa
data_meanwage <- data_meanwage[!(ref_area == "ZAF" & !(source %in% c("DA:1811","BA:595"))),]

# order mean wage by country, year
data_meanwage <- data_meanwage[order(ref_area, year),]

# write it into excel
# write.csv(data_meanwage, file="mw.csv", row.names = F)

#
#

# read the manually adjusted data in excel
data_mw <- data.table(read.csv(file="Auxiliary data/ILO meanwage final.csv", header = T, stringsAsFactors = T))

#
#

# checking the duplicit values

# dup1 <- duplicated(data_meanwage[,.(ref_area,indicator,type,year)])
# dup2 <- duplicated(data_meanwage[,.(ref_area,indicator,type,year)], fromLast = T)
# data_duplicates <- data_meanwage[dup1 | dup2,]
# data_duplicates_a <- data_duplicates[order(ref_area,year),]
# View(data_duplicates_a)