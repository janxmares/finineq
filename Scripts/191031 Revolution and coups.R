# Read data on revolutions and coups
# Jan Mares, 191031

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(states)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#

revcoups <- data.table(read.table("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_ccode_year.txt", sep="\t", header = T, stringsAsFactors = F))

# assign 
revcoups[, iso3c := countrycode(ccode,'gwn','iso3c')]

# get the required columns
revcoups <- revcoups[, j = .(iso3c, year, RevCoups = coup1 + coup2 + coup3 + coup4)]

# there are some NAs for iso3 codes, but none of them contains revcoups
# drop them
revcoups <- revcoups[!is.na(iso3c),]

# only keep observations since 1980
revcoups <- revcoups[year >= 1980,]

# write into file
write.csv(revcoups, file = "Auxiliary data/to merge/revcoups.csv", row.names = F)