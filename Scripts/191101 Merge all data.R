# Merge all sources of data
# Jan Mares, 191101

# Libraries
library(data.table)
library(countrycode)
library(zoo)
library(BMS)
library(imputeTS)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# read all the underlying data
wid <- data.table(read.csv(file = "Auxiliary data/to merge/wid data.csv", header = T, stringsAsFactors = F))
wb <- data.table(read.csv(file = "Auxiliary data/to merge/wb data.csv", header = T, stringsAsFactors = F))
waryears <- data.table(read.csv(file = "Auxiliary data/to merge/waryears.csv", header = T, stringsAsFactors = F))
wages.oecd <-  data.table(read.csv(file = "Auxiliary data/to merge/wages oecd.csv", header = T, stringsAsFactors = F))
minwage.ilo <- data.table(read.csv(file = "Auxiliary data/to merge/minwage ILO.csv", header = T, stringsAsFactors = F))
meanwage.ilo <- data.table(read.csv(file = "Auxiliary data/to merge/meanwage ILO.csv", header = T, stringsAsFactors = F))
ud.ilo <- data.table(read.csv(file = "Auxiliary data/to merge/union density ILO.csv", header = T, stringsAsFactors = F))
revcoups <- data.table(read.csv(file = "Auxiliary data/to merge/revcoups.csv", header = T, stringsAsFactors = F))
inv.shares <- data.table(read.csv(file = "Auxiliary data/to merge/investment shares detail.csv", header = T, stringsAsFactors = F))
chinnito <- data.table(read.csv(file = "Auxiliary data/to merge/chinnito.csv", header = T, stringsAsFactors = F))
findev <- data.table(read.csv(file = "Auxiliary data/to merge/findev.csv", header = T, stringsAsFactors = F))
findev.orig <- data.table(read.csv(file = "Auxiliary data/to merge/findev orig.csv", header = T, stringsAsFactors = F))
efw <- data.table(read.csv(file = "Auxiliary data/to merge/efw2017paneldata.csv", header = T, stringsAsFactors = F))
educindex <- data.table(read.csv(file = "Auxiliary data/to merge/educindex UN.csv", header = T, stringsAsFactors = F))
clpr <- data.table(read.csv(file = "Auxiliary data/to merge/CLandPR.csv", header = T, stringsAsFactors = F))
barrolee <- data.table(read.csv(file = "Auxiliary data/to merge/bl.csv", header = T, stringsAsFactors = F))
leftwing <- data.table(read.csv(file = "Auxiliary data/to merge/IDB leftwing.csv", header = T, stringsAsFactors = F))
swiid <- data.table(read.csv(file = "Auxiliary data/to merge/swiid.csv", header = T, stringsAsFactors = F))
icrg <- data.table(read.csv(file = "Auxiliary data/to merge/icrg.csv", header = T, stringsAsFactors = F))
glob <- data.table(read.csv(file = "Auxiliary data/to merge/kof.csv", header = T, stringsAsFactors = F))
medage <- data.table(read.csv(file = "Auxiliary data/to merge/median age.csv", header = T, stringsAsFactors = F))


#
#

# checks

# head(wid)
# head(wb)
# head(waryears)
# head(wages.oecd)
# head(minwage.ilo)
# head(meanwage.ilo)
# head(ud.ilo)
# head(revcoups)
# head(inv.shares)
# head(chinnito)
# head(findev)
# head(findev.orig)
# head(efw)
# head(educindex)
# head(clpr)
# head(barrolee)
# head(leftwing)
# head(icrg)
# head(glob)
# head(icrg)

# merge

#
#

data <- merge(wid, wb, by = c("iso3c","year"), all = T)
data <- merge(data, waryears, by = c("iso3c","year"), all = T)
data <- merge(data, wages.oecd, by = c("iso3c","year"), all = T)
data <- merge(data, minwage.ilo, by = c("iso3c","year"), all = T)
data <- merge(data, meanwage.ilo, by = c("iso3c","year"), all = T)
data <- merge(data, ud.ilo, by = c("iso3c","year"), all = T)
data <- merge(data, revcoups, by = c("iso3c","year"), all = T)
data <- merge(data, inv.shares, by = c("iso3c","year"), all = T)
data <- merge(data, chinnito, by = c("iso3c","year"), all = T)
data <- merge(data, findev, by = c("iso3c","year"), all = T)
data <- merge(data, findev.orig, by = c("iso3c","year"), all = T)
data <- merge(data, efw, by = c("iso3c","year"), all = T)
data <- merge(data, educindex, by = c("iso3c","year"), all = T)
data <- merge(data, clpr, by = c("iso3c","year"), all = T)
data <- merge(data, barrolee, by = c("iso3c","year"), all = T)
data <- merge(data, leftwing, by = c("iso3c","year"), all = T)
data <- merge(data, swiid, by = c("iso3c","year"), all = T)
data <- merge(data, icrg, by = c("iso3c","year"), all = T)
data <- merge(data, glob, by = c("iso3c","year"), all = T)
data <- merge(data, medage, by = c("iso3c","year"), all = T)

# corrected for the missing waryears, revcoups, leftwign obs.
data[is.na(WarYears), WarYears := 0]
data[is.na(RevCoups), RevCoups := 0]
data[is.na(LeftWing), LeftWing := 0]

# checks
# View(data)
# View(data[duplicated(data[, j = .(iso3c,year)]),]) # should be NULL table

# write final data
write.csv(data, file = "data_hm.csv", row.names = F)