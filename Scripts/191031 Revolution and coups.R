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

# get country codes from Gleditsch and Ward (1999)
gwstates <- as.data.table(gwstates)
gwcodes <- gwstates[, j = .(ccode = gwcode, iso3c)]
gwcodes <- gwcodes[!(duplicated(gwcodes$ccode)),] # some states are duplicate as they begin and cease to exist

# merge with revcoups data
revcoups <- merge(revcoups, gwcodes, by = c("ccode"), all = T)

#
#

# get the required columns
revcoups <- revcoups[, j = .(iso3c, year, RevCoups = coup1 + coup2 + coup3 + coup4)]

# there are some NAs for iso3 codes, but none of them contains revcoups
# drop them
revcoups <- revcoups[!is.na(iso3c),]

# only keep observations since 1980
revcoups <- revcoups[year >= 1980,]

# write into file
write.csv(revcoups, file = "Auxiliary data/to merge/revcoups.csv", row.names = F)