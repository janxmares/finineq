# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#
#

# read the data from WID

#
#

data_wid <- data.table(read.csv("Auxiliary data/WID_Data_29102019.csv", sep = ";", skip = 1, header = F, stringsAsFactors = F))

# Keep required columns
data_wid <- data_wid[, j = .(country = V1, type = V3, year = V4, value = as.numeric(V5))]

# remove rows which cause troubles later as they disrupt assign of iso3 codes
data_wid <- data_wid[!(country %in% c("Rural China","Urban China","Indiana")),]


# assign iso3 codes
data_wid[, iso3c := countrycode(country, "country.name", "iso3c")]

# keep only non-NA iso codes
data_wid <- data_wid[!(is.na(iso3c)),]

# drop country column
data_wid[, country := NULL]

# # check
# nrow(data_wid)
# head(data_wid)

# reshape data to wide
data_wid_w <- data.table(dcast(data_wid, iso3c + year ~ type, value.var = "value"))

# rename value columns
setnames(data_wid_w, c("p90p100","p99p100"), c("Top10share","Top1share"))

#
#

# write data
write.csv(data_wid_w, file = "Auxiliary data/to merge/wid data.csv", row.names = F)


# # examples 
# data_wid[iso3c == "RUS",]
# View(data_wid[iso3c == "RUS",])
# View(data_wid[iso3c == "USA",])

#
#

# determine available countries

# data <- data.table(read.csv(file="WID data top income shares.csv", header=T, stringsAsFactors=F))

# # Check
# # View(data)

# # Creates a list of ID columns split by \n => 3 elements where the 3rd is the type of income share
# perc_id <- strsplit(data$ID, "\n")

# # Take 3rd element of every member of the list
# vec <- unlist(lapply(perc_id, function(x){
# 		x[3]
# 		}))


# # Add it to the data
# data[, Type := vec]

# # Drop the ID var
# data[, ID:=NULL]

# # Save as csv
# write.csv(data, file="WID data top income shares FINAL.csv", row.names=F)

# # Tryouts
# data_available <- data[!(is.na(Value)),]

# # View(data_available)
# unique(data_available$Country)
