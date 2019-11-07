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

# corrected for the missing waryears, revcoups, leftwign obs.
data[is.na(WarYears), WarYears := 0]
data[is.na(RevCoups), RevCoups := 0]
data[is.na(LeftWing), LeftWing := 0]

# checks
# View(data)
# View(data[duplicated(data[, j = .(iso3c,year)]),]) # should be NULL table

# write final data
write.csv(data, file = "data_hm.csv", row.names = F)


#
#

# analysis

#
#

# define time periods, 3 year
data_f <- data[year %in% c(2000:2015),]
data_f[year %in% c(2000:2003), period := 1]
data_f[year %in% c(2004:2006), period := 2]
data_f[year %in% c(2007:2009), period := 3] 
data_f[year %in% c(2010:2012), period := 4]
data_f[year %in% c(2013:2015), period := 5]

data_f[, c("country","year") := NULL]

# define time periods, 5 year
data_f <- data[year %in% c(2001:2015),]
data_f[year %in% c(2001:2005), period := 1]
data_f[year %in% c(2006:2010), period := 2]
data_f[year %in% c(2011:2015), period := 3]

data_f[, c("country","year") := NULL]
# View(data_f)

# average observations by period and country
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","period")]

# drop observations for which we do not have the depenedent variale
data_f <- data_f[!is.nan(Top10share), ]
data_f <- data_f[!is.na(Top10share), ]

View(data_f[duplicated(data_f[, j = .(iso3c,year)]),])
str(data_f)

# filter only obs with available barrolee data
data_f <- data_f[!is.nan(PrEdu), ]

# filter only countries available in BARRO LEE
data_f <- data_f[iso3c %in% unique(barrolee$iso3c),]
data_f <- data_f[!(iso3c %in% c("CIV","URY")),]

# interpolate education attainment na.interpolation
data_f[, PrEdu := na.interpolation(PrEdu, option="linear"), by=c("iso3c")]
data_f[, SecEdu := na.interpolation(SecEdu, option="linear"), by=c("iso3c")]
data_f[, TerEdu := na.interpolation(TerEdu, option="linear"), by=c("iso3c")]

View(data_f)


# how many observations are NAN for individual variables?
sapply(data_f, function(x){sum(is.nan(x))})
sapply(data_try, function(x){sum(is.na(x))})

# columns to drop, most missing values
drops <- c("NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "MetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI 
		   "WarYears","RevCoups") # invariant in the sample


data_try <- data_f[, c("NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "ExpMetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI ) := NULL]
		   # "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation") := NULL] # subcomponents of EFW

# first eliminate the countries where all the three period are missing for a variable
drop_c <- c("ARE","TWN","BHR","BEN","BDI","BDI","CAF","GMB","IRQ","LBR","LSO",
			"LUX","MRT","MUS","SDN","ZWE","YEM") 

data_try <- data_try[!(iso3c %in% drop_c), ]

View(data_try)


data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean), by = c("iso3c")]

data_dm[, c("iso3c","Top1share") := NULL]

data_dm <- data_dm[complete.cases(data_dm), ]

View(data_dm)

bma <- bms(data_dm, iter=2000000, burn=100000, mprior = "uniform", g = "hyper",
                           nmodel=5000, mcmc="bd", user.int = F)
coef(bma, exact = T)
summary(bma)

check_sd <- data_dm[, lapply(.SD, function(x){sd(x)})]
check_sd
View(cor(data_dm))
sum(complete.cases(data_dm))
nrow(data_dm)


data_try_c <- data_try[complete.cases(data_try), ]

data_try_c[, c("country","year","WarYears","RevCoups","LeftWing") := NULL]

data_dm <- data_try_c[, lapply(.SD[,1:ncol(.SD)], demean), by = c("iso3c")]

data_dm[, c("iso3c","Top1share","PrEdu","SecEdu") := NULL]

View(data_dm)
ncol(data_dm)

View(data_try_c)