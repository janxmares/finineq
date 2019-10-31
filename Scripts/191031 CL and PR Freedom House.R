# Read data on civil liberties and political rights (freedom house)
# Jan Mares, 191031

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(XLConnect) # read xls files
library(stringr)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#
#

# Load worksheet
freedomh_wb <-  loadWorkbook("Auxiliary data/freedomh.xlsx")
fh <- data.table(readWorksheet(freedomh_wb, sheet="Country Ratings, Statuses ", startRow=3, startCol=1), stringsAsFactors=F)

# rename the country column
setnames(fh, c("Col1"), c("country"))

# Melt the data frame to long format
fhlong <- melt(fh, id.vars=c("country"), value.name="value", variable.name="category")

# check
# View(fhlong)

# Extract the numbers from category column to properly assign years
yr_int <- str_extract(fhlong$category, "[0-9]+")
yr_int[is.na(yr_int)] <- 0

# Assign it to data
fhlong$year <- 1972 + as.numeric(yr_int)

# Get rid of numbres in categories
fhlong$category <- str_extract(fhlong$category,"[:alpha:]+")

# Reshape to wide format
fhwide <- data.table(dcast(fhlong,  country + year ~ category))

# Get rid of status column
fhwide[,Status:=NULL]

# Convert variables to numeric
fhwide$CL <- as.numeric(fhwide$CL)
fhwide$PR <- as.numeric(fhwide$PR)

# Assign iso3c code to observations
fhwide[,iso3c:=countrycode(fhwide$country,"country.name","iso3c")]

# List missing observations
# fhwide[is.na(fhwide$CL),]

# Trouble with east and west Germany, they are assigned the same iso code, same with Vietnam, Yemen, Yugoslavia
# SOLUTION
	# Drop instances, where the data is NA (gets rid of double observations)
	# They are added eventualy with the merge to main data anyway

fhwide <- fhwide[!(is.na(CL)),]
fhwide <- fhwide[country!="Germany, E. "]

# Construct final structure
fh_m <- fhwide[,j=list(CivLib=CL,PolRights=PR), by=c("iso3c","year")]

fh_m[,CLandPR:=(CivLib+PolRights)/2]
fh_m[,c("CivLib","PolRights"):=NULL]

# write into file
write.csv(fh_m, file = "Auxiliary data/CLandPR.csv", row.names = F)