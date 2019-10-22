# Union density from ILO
# Jan Mares
# 3/12/2018
# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)
library(Rilostat)


# Set the working directory
wd <- c("c:/Users/JM/Dropbox/Research/Finance and income inequality/")
setwd(wd)

a <- get_ilostat_toc(search="union")

data_ilo_ud <- data.table(get_ilostat("ILR_TUMT_NOC_RT_A"))
data_ilo_minwage <- data.table(get_ilostat("EAR_4MNP_NOC_NB_A"))
data_ilo_meanwage <- data.table(get_ilostat("EAR_4MTH_SEX_ECO_CUR_NB_A"))

#
#

## minimum wage

# filter required columns
data_minwage <- data_ilo_minwage[, .(ref_area, indicator, time, obs_value)]
setnames(data_minwage, c("iso3c","indicator","year","value"))

# data_minwage[, country := countrycode(iso3c, "iso3c", "country.name")] # test iso3 codes

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







View(data_meanwage)

# checking the duplicit values

# dup1 <- duplicated(data_meanwage[,.(ref_area,indicator,type,year)])
# dup2 <- duplicated(data_meanwage[,.(ref_area,indicator,type,year)], fromLast = T)
# data_duplicates <- data_meanwage[dup1 | dup2,]
# data_duplicates_a <- data_duplicates[order(ref_area,year),]
# View(data_duplicates_a)

#
#

# there are multiple sources of the data in several countries. In some, the values differ
# significantly. We therefore take source which provide the most recent observation
data_sel_recent <- data_meanwage[, .(nobs = .N), by = c("source","ref_area")]


data_sel_recent <- data_sel_recent[order(ref_area,-nobs), ]
data_sel_recent[, nobs := NULL]

#
#


View(data_meanwage[ref_area=="CZE",])
View(data_ilo_ud[ref_area=="CZE",])


unique(data_ilo_ud$ref_area)
View(data_ilo_meanwage)
