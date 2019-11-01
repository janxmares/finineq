# Read data on financial indicators from GFDD
# Jan Mares, 191031

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(XLConnect)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# load data
findev_gfdd <- loadWorkbook("Auxiliary data/October2019gfdd.xlsx")
findev <- data.table(readWorksheet(findev_gfdd, sheet="Data - October 2019", startRow=1, startCol=1))

# filter required columns
findev <- findev[, j = .(iso3c = iso3, year, gfddai01,gfddai02, gfddam01, gfdddi01, gfdddm01,
						 gfddei01, gfddem01, gfddsi01, gfddsm01)]

# rename the variables
setnames(findev, c("gfddai01","gfddai02","gfddam01","gfdddi01","gfdddm01","gfddei01","gfddem01",
				   "gfddsi01","gfddsm01"),
				 c("BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap","NIM",
				   "MarketTurn","ZScore","StockVol"))

# only observations past 1980
findev <- findev[year >= 1980, ]

# write data into file
write.csv(findev, file = "Auxiliary data/to merge/findev orig.csv", row.names = F)