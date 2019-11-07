# Read data on regional dummies
# Jan Mares, 191105

# Libraries
library(data.table)
library(countrycode)
library(stringr)
library(XLConnect)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#

# Load list of Sub-saharan and Latin American countries
wbreg <- loadWorkbook("Auxiliary data/Regions.xlsx")

# Load sheet with SubSaharan countries
SubS <- data.table(readWorksheet(wbreg, sheet = "Sub-saharan Africa", startRow = 1, startCol = 1))

# Fucntion to concatenate list of string created by str_extract
concat_str <- function(x){
	name <- character(0)
	if(length(x)>1){
		for (i in 1:length(x)) {
			if(i==1){
			name <- paste(name,x[i],sep="")	
			}
			else{
			name <- paste(name,x[i],sep=" ")		
			}
		}
	}
	else{
		name <- x
	}
	return(name)
}

# Extract the country names into the list
aid_names <- str_extract_all(SubS$Wiki,"[:alpha:]+")

# Assign country names and iso3c
SubS$Wiki <- sapply(aid_names, concat_str)
SubS[, iso3c:=countrycode(Congress,"country.name","iso3c")]

# filter required columns
SubS <- SubS[, j = .(iso3c, SubSahara)]

#
#

# Load sheet with LatAm countries
LatAm <- data.table(readWorksheet(wbreg, sheet="Latin America", startRow=1, startCol=1))

# Assign iso3c
LatAm[, iso3c := countrycode(Wiki,"country.name","iso3c")]

# drop NAs (french territories in Americas)
LatAm <- LatAm[!is.na(iso3c), ]

# filter required columns
LatAm <- LatAm[, j = .(iso3c, LatAm)]

#
#

# Merge regional dummes together
reg <- merge(SubS, LatAm, by = c("iso3c"), all = T)

# write data into file
write.csv(reg, file = "Auxiliary data/to merge/regions.csv", row.names = F)