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

# create column names for the data
yrs <- c(1972:2017)
pr <- paste0(rep("PR_", length(yrs)), yrs)
cl <- paste0(rep("CL_", length(yrs)), yrs)
status <- paste0(rep("Status_", length(yrs)), yrs)

coln <- data.table(coln = c(pr,cl,status))
coln[, year := str_extract(coln, "[0-9]+")]

# change the order to reflect years
coln <- coln[order(year),]

# set column names
setnames(fh, c("country", coln$coln))

# check
# coln
# View(fh)

# Melt the data frame to long format
fhlong <- melt(fh, id.vars=c("country"), value.name="value", variable.name="category")

# Extract years from the category column
fhlong[, year := str_extract(category, "[0-9]+")]

# Get rid of numbres in categories
fhlong$category <- str_extract(fhlong$category,"[:alpha:]+")

# Reshape to wide format
fhwide <- data.table(dcast(fhlong,  country + year ~ category))

# Get rid of status column
fhwide[,Status := NULL]

# Convert variables to numeric
fhwide$CL <- as.numeric(fhwide$CL)
fhwide$PR <- as.numeric(fhwide$PR)

# Assign iso3c code to observations
fhwide[,iso3c := countrycode(fhwide$country,"country.name","iso3c")]

# drop NA iso3c codes - Czechoslovakia, Serbia & Montenegro, Kosovo, Micronesia
fhwide <- fhwide[!is.na(iso3c), ]

# Trouble with east and west Germany, they are assigned the same iso code, same with Russia (USSR),
# Vietnam, Yemen, Yugoslavia
# SOLUTION
	# Drop instances, where the data is NA (gets rid of double observations)
	# They are added eventualy with the merge to main data anyway

fhwide <- fhwide[!(is.na(CL)),]
fhwide <- fhwide[!(country %in% c("Germany, E. ","USSR")),]

# Construct final structure
fh_m <- fhwide[, j = .(CivLib = CL, PolRights = PR), by = c("iso3c","year")]

fh_m[, CLandPR := (CivLib + PolRights) / 2]
fh_m[, c("CivLib", "PolRights") := NULL]

# only observations past 1980
fh_m <- fh_m[year >= 1980, ]

# write into file
write.csv(fh_m, file = "Auxiliary data/to merge/CLandPR.csv", row.names = F)