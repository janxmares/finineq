# Libraries
library('WDI')
library('data.table')
library('countrycode')
library('XLConnect')
library('stringr')

# set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# load ICRG workbook
icrg_wb <- loadWorkbook("Auxiliary data/3BResearchersDataset2018.xls")

#
#

# load data on corruption
icrg_corr <- data.table(readWorksheet(icrg_wb, sheet="F-Corruption", startRow=8, startCol=1))

# Melt into long format
icrg_corr_l <- data.table(melt(icrg_corr, id.vars = c("Country"), variable.name = "year", value.name = "corr"))

# drop East Germany and USSR
icrg_corr_l <- icrg_corr_l[!(Country %in% c("West Germany","East Germany","USSR")),]

# adjust year column
icrg_corr_l[, year := as.numeric(str_replace(year, "X",""))]

# assign iso3 code
icrg_corr_l[, iso3c := countrycode(Country, "country.name", "iso3c")]
icrg_corr_l[, Country := NULL]

icrg_corr_l <- icrg_corr_l[!is.na(iso3c),]

#
#

# load data on bureaucracy quality
icrg_burreau <- data.table(readWorksheet(icrg_wb, sheet="L-Bureaucracy Quality", startRow=8, startCol=1))

# Melt into long format
icrg_burreau_l <- data.table(melt(icrg_burreau, id.vars = c("Country"), variable.name = "year", value.name = "burreau"))

# drop East Germany and USSR
icrg_burreau_l <- icrg_burreau_l[!(Country %in% c("West Germany","East Germany","USSR")),]

# adjust year column
icrg_burreau_l[, year := as.numeric(str_replace(year, "X",""))]

# assign iso3 code
icrg_burreau_l[, iso3c := countrycode(Country, "country.name", "iso3c")]
icrg_burreau_l[, Country := NULL]

icrg_burreau_l <- icrg_burreau_l[!is.na(iso3c),]

#
#

# load data on democratic accountability
icrg_demacc <- data.table(readWorksheet(icrg_wb, sheet="K-Democratic Accountability", startRow=8, startCol=1))

# Melt into long format
icrg_demacc_l <- data.table(melt(icrg_demacc, id.vars = c("Country"), variable.name = "year", value.name = "demAcc"))

# drop East Germany and USSR
icrg_demacc_l <- icrg_demacc_l[!(Country %in% c("West Germany","East Germany","USSR")),]

# adjust year column
icrg_demacc_l[, year := as.numeric(str_replace(year, "X",""))]

# assign iso3 code
icrg_demacc_l[, iso3c := countrycode(Country, "country.name", "iso3c")]
icrg_demacc_l[, Country := NULL]

icrg_demacc_l <- icrg_demacc_l[!is.na(iso3c),]

#
#

# load data on law and order
icrg_laworder <- data.table(readWorksheet(icrg_wb, sheet="I-Law and Order", startRow=8, startCol=1))

# Melt into long format
icrg_laworder_l <- data.table(melt(icrg_laworder, id.vars = c("Country"), variable.name = "year", value.name = "lawOrder"))

# drop East Germany and USSR
icrg_laworder_l <- icrg_laworder_l[!(Country %in% c("West Germany","East Germany","USSR")),]

# adjust year column
icrg_laworder_l[, year := as.numeric(str_replace(year, "X",""))]

# assign iso3 code
icrg_laworder_l[, iso3c := countrycode(Country, "country.name", "iso3c")]
icrg_laworder_l[, Country := NULL]

icrg_laworder_l <- icrg_laworder_l[!is.na(iso3c),]

#
#

# merge icrg together
icrg_f <- merge(icrg_corr_l, icrg_burreau_l, by = c("iso3c", "year"))
icrg_f <- merge(icrg_f, icrg_demacc_l, by = c("iso3c", "year"))
icrg_f <- merge(icrg_f, icrg_laworder_l, by = c("iso3c", "year"))

# check
# head(icrg_f)
# View(icrg_f) 

# write data into file
write.csv(icrg_f, file = "Auxiliary data/to merge/icrg.csv", row.names = F)