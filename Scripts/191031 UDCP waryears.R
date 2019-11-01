# Read data on war years from UDCP database
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

# Data on wars 
# Loading worksheet
wbwars <- loadWorkbook("Auxiliary data/ucdp-prio-acd-191.xlsx")
wars <- data.table(readWorksheet(wbwars,sheet="UcdpPrioConflict_v19_1", startRow=1, startCol=1), stringsAsFactors=F)

# Only the conflict years where # of casulties is > 1000
wars <- wars[intensity_level==2,]

# Only relevant columns
wars <- wars[,j=list(year=year,location=location,type=type_of_conflict)]

#

for(i in 1:nrow(wars)){
	wars$state1[i] <- strsplit(wars$location,",")[[i]][1]	
	wars$state2[i] <- strsplit(wars$location,",")[[i]][2]	
	wars$state3[i] <- strsplit(wars$location,",")[[i]][3]	
	wars$state4[i] <- strsplit(wars$location,",")[[i]][4]	
	wars$state5[i] <- strsplit(wars$location,",")[[i]][5]	
	wars$state6[i] <- strsplit(wars$location,",")[[i]][6]
}

# Function to get rid of space (first occurance)
repl_space <- function(x){
	str_replace(x," ","")	
}

# Get adjusted conflict country names
confnew <- apply(wars[,j=list(state2=state2,state3=state3,state4=state4,
							  state5=state5,state6=state6)], 2, repl_space)

# Drop the former columns
wars[,c("state2","state3","state4","state5","state6"):=NULL]

# Bind with the adjusted
wars <- cbind(wars,confnew)
wars[, location := NULL]

warslong <- data.table(melt(wars, id.vars=c("year","type")))
setkey(warslong, value)

# Drop the NAs
warslong <- warslong[!is.na(warslong$value),]

# Drop redundant columns
warslong[, c("type","variable"):=NULL]

# Assign iso3 codes
warslong$iso3c <- countrycode(warslong$value, "country.name", "iso3c")

# Assign iso3c code to Yemen as it is unavailable
warslong[value=="Yemen (North Yemen)", iso3c:="YMD"]
warslong[value=="South Yemen", iso3c:="YMD"]

# drop Hyderabad rows
warslong <- warslong[!(value %in% c("Hyderabad")),]

# 
# Sum the conflicts across country-years
waryears <- warslong[year>=1980, j=.(WarYears=.N), by=c("year","iso3c")]

# 
# View(waryears)

# write into file
write.csv(waryears, file = "Auxiliary data/to merge/waryears.csv", row.names = F)

# Bookkeeping
rm(list=c("wbwars"))