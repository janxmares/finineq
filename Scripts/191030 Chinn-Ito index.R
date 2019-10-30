# Read Chinn-Ito financial opennes index
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(readstata13)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# Data on Chinn-Ito financial openness index
chinnito <- data.table(read.dta13("Auxiliary data/kaopen_2017.dta"))

str(chinnito)
head(chinnito)
View(chinnito)

#
#

# filter required columns
chinnito <- chinnito[, j = .(iso3c = ccode, year, kaopen)]

#
#

# write into file
write.csv(chinnito, file = "Auxiliary data/chinnito.csv", row.names = F)