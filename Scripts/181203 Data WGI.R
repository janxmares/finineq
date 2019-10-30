# Governance data from World Bank, WGI
# Jan Mares
# 3/12/2018

# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)
library(devtools)
library(WDI)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# Define governance indicators 
wgi.indices <- c("CC.EST","GE.EST","PV.EST","RQ.EST","RL.EST","VA.EST")

# Read first dataset to obtain selected country codes
wbdata <- data.table(WDI(indicator=wgi.indices, start=1996, end=2017))

# Selection of countries to drop from the analysis
codedrop <- c("1A","1W","4E","7E","8S","B8","EU","F1","V1","V2","V3","V4","T2","T3","T4","T5","T6","T7",
				   "AN","FM","FO","IM","JG","KI","KM","KN","KV","KY","LC","MF","MH","MP","OE","PF","PS","S1",
				   "S2","S3","S4","SB","SC","SM","ST","SV","SX","TC","TL","TV","VC","VI","XC","XD","XE",
				   "XF","XG","XH","XI","XJ","XL","XL","XM","XN","XO","XP","XQ","XR","XS","XT","XK","XU",
				   "XY","Z4","Z7","ZF","ZG","ZJ","ZQ","ZT","VG","NR","","..")

# Define selected country codes
selcodes <- unique(wbdata$iso2c[!(wbdata$iso2c %in% codedrop)])

# Total natural resources rents
wbdatasub <- wbdata[iso2c %in% selcodes,]

# Final data
data.wgi <- wbdatasub

# Check
# View(data.wgi)

# Write data into csv file
write.csv(data.wgi, file="Auxiliary data/WGI indices.csv", row.names=F)
