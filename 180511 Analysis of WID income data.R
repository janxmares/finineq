# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

data <- data.table(read.csv(file="WID data top income shares.csv", header=T, stringsAsFactors=F))

# Check
# View(data)

# Creates a list of ID columns split by \n => 3 elements where the 3rd is the type of income share
perc_id <- strsplit(data$ID, "\n")

# Take 3rd element of every member of the list
vec <- unlist(lapply(perc_id, function(x){
		x[3]
		}))


# Add it to the data
data[, Type := vec]

# Drop the ID var
data[, ID:=NULL]

# Save as csv
write.csv(data, file="WID data top income shares FINAL.csv", row.names=F)

# Tryouts
data_available <- data[!(is.na(Value)),]

# View(data_available)
unique(data_available$Country)

#
#

# read the data from WID

#
#

data_wid <- data.table(read.csv("WID_Data_29102019.csv", sep = ";", skip = 1, header = F, stringsAsFactors = F))

# Keep required columns
data_wid <- data_wid[, j = .(Country = V1, Type = V3, year = V4, value = as.numeric(V5))]

data_wid[, iso3c := countrycode(Country, "country.name", "iso3c")]

# keep only non-NA iso codes
data_wid <- data_wid[!(is.na(iso3c)),]

# check
nrow(data_wid)
View(data_wid)

#
#

unique(data_wid$iso3c)

# examples 
data_wid[iso3c == "RUS",]
View(data_wid[iso3c == "RUS",])
View(data_wid[iso3c == "USA",])
names(data_wid)
#
#

sum(is.na(data_wid$value))