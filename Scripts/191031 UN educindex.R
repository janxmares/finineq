# Read un education index, UN
# Jan Mares, 191031

# Libraries
library(data.table)
library(countrycode)
library(XLConnect)
library(stringr)


# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# education index
# load worksheet
educindex_un <- loadWorkbook("Auxiliary data/educindex UN.xlsx")
educindex <- data.table(readWorksheet(educindex_un, sheet="Sheet1", startRow=1, startCol=1), stringsAsFactors=F)

# reshape to long format
educindex <- data.table(melt(educindex, id.vars = "Country", variable.name = "year", value.name = "EducIndex"))

# adjust year and change variables to numeric
educindex[, year := as.numeric(str_replace(year, "X",""))]
educindex[, EducIndex := as.numeric(EducIndex)]

# assign iso3 codes
educindex[, iso3c := countrycode(Country, "country.name","iso3c")]

# correct for Eswatini (Swaziland)
educindex[Country == "Eswatini (Kingdom of)", iso3c := "SWZ"]

# delete country column
educindex[, Country := NULL]

# write data into file
write.csv(educindex, file = "Auxiliary data/educindex UN.csv", row.names = F)