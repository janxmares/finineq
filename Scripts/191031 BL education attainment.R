# Read data on control variables from WB
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(WDI)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# Barro and Lee data on education (2015 update)
bl <- data.table(read.csv(file="Auxiliary data/BL2013_MF1599_v2.2.csv", header=T, stringsAsFactors=F))
setnames(bl, "WBcode", "iso3c")

#View(bl)
# 5-year intervals
bl <- bl[year>=1980,j=.(lpc=lpc, lsc=lsc, lhc=lhc), by=c("country","year","iso3c")]

# Change variables names
setnames(bl, c("lpc","lsc","lhc"),c("PrEdu","SecEdu","TerEdu"))

# Drop redundant column
bl[,c("country"):=NULL]

# View(bl)

#
#

# Add data from projections
# Barro and Lee data on education, projection
bl_fcast <- data.table(read.csv(file="Auxiliary data/OUP_proj_MF1564_v1.csv", header=T, stringsAsFactors=F))
setnames(bl_fcast, "WBcode", "iso3c")

# 5-year intervals
bl_fcast <- bl_fcast[year>=1980,j=.(lpc=lpc, lsc=lsc, lhc=lhc), by=c("country","year","iso3c")]

# Change variabl_fcastes names
setnames(bl_fcast, c("lpc","lsc","lhc"),c("PrEdu","SecEdu","TerEdu"))

# Drop redundant column
bl_fcast[,c("country"):=NULL]

# Only keep 2015 fcast
bl_fcast <- bl_fcast[year == 2015, ]

#
#

# bind with the historic values
bl_final <- rbind(bl, bl_fcast)
bl_final <- bl_final[order(iso3c, year), ]

# only observations past 1980
bl_final <- bl_final[year >= 1980, ]

# write data into file
write.csv(bl_final, file = "Auxiliary data/to merge/bl.csv", row.names = F)