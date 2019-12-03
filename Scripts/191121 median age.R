# Libraries
library('data.table')
library('countrycode')
library('here')
library('XLConnect')
library('stringr')

#
#

# Load the workbook with median age data
wbage <- loadWorkbook("Auxiliary data/WPP2019_POP_F05_MEDIAN_AGE.xlsx")

# Load sheet with SubSaharan countries
medage <- data.table(readWorksheet(wbage, sheet = "ESTIMATES", startRow = 18, startCol = 2, header = F))

# drop redundant columns
medage[, c("Col1","Col3","Col5","Col6") := NULL]

# set the proper names
setnames(medage, c('country', 'ccode', 'x1950', 'x1955', 'x1960', 'x1965', 'x1970',
                   'x1975','x1980','x1985','x1990','x1995','x2000','x2005',
                   'x2010','x2015','x2020'))

# assign proper iso3 codes
medage[, iso3c := countrycode(ccode, "un", "iso3c")]

# keep only the observations where iso3 codes are available (drop regions etc/)
medage <- medage[!is.na(iso3c), ]
medage[, c('ccode') := NULL]

# transform into long format
medage <- melt(medage, id.vars = c('country','iso3c'), value.name = 'Age')
medage[, year := as.numeric(str_replace(variable, "x", ""))]
medage[, c('country','variable') := NULL]

# only keep the years 1980+
medage <- medage[year >= 1980, ]

# write csv
write.csv(medage, file = "Auxiliary data/to merge/median age.csv", row.names = F)