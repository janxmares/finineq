# Read financial development indicators, Svyridzenka et al. (2016)
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(readstata13)
library(XLConnect)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# financial indicators
# load worksheet
findev_wb <- loadWorkbook("Auxiliary data/financial_ind_wb.xlsx")
findev <- data.table(readWorksheet(findev_wb,sheet="Sheet1", startRow=1, startCol=1), stringsAsFactors=F)

# filter required columns
findev <- findev[,j=.(iso3c=code, year=year, FID=FID,
											 FIA=FIA,
											 FIE=FIE,
											 FMD=FMD,
											 FMA=FMA,
											 FME=FME)]

# replace 0 values (unobserved) with NAs
findev[FID == 0, FID := NA]
findev[FIA == 0, FIA := NA]
findev[FIE == 0, FIE := NA]
findev[FMD == 0, FMD := NA]
findev[FMA == 0, FMA := NA]
findev[FME == 0, FME := NA]

# only observations past 1980
findev <- findev[year >= 1980, ]

# write into file
write.csv(findev, file = "Auxiliary data/to merge/findev.csv", row.names = F)