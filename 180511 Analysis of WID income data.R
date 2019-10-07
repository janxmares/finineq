# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)

# Set the working directory
wd <- c("c:/Users/JM/Dropbox/Research/Finance and Income Inequality/")
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
data[,Type:=vec]

# Drop the ID var
data[,ID:=NULL]

# Save as csv
write.csv(data, file="WID data top income shares FINAL.csv", row.names=F)

# Tryouts
data_available <- data[!(is.na(Value)),]

# View(data_available)
unique(data_available$Country)