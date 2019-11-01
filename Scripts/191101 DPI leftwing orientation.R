# Leftwing political orientation, database on political institutions, IDB
# Jan Mares
# 1/11/2019

# Libraries
library(data.table)
library(countrycode)


# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# read data
dpi <- data.table(read.csv(file = "Auxiliary data/DPI2017_basefile_Jan2018.csv",
				           header = T, stringsAsFactors = F))

# select columns
dpi <- dpi[, j = .(iso3c = ifs, year, execrlc)]

# execlrc - 3 is leftwing, create a 0/1 variable
dpi[execrlc == 3, LeftWing := 1]
dpi[, execrlc := NULL]

# drop iso3c == 1, Turkish part of Cyprus
dpi <- dpi[iso3c != "0"]

# filter observations past 1980
dpi <- dpi[year >= 1980, ]

# write into file
write.csv(dpi, file = "Auxiliary data/to merge/IDB leftwing.csv", row.names = F)