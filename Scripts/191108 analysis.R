# Preliminary analysis, market gini coefficient
# Jan Mares, 191108

# Libraries
library(data.table)
library(countrycode)
# library(zoo)
library(dilutBMS2)
library(imputeTS)
library(here)
library(dummies)

# # Set the working directory
# wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
# setwd(wd)

# read data file
data <- data.table(read.csv(file = here('data_hm.csv'), header = T, stringsAsFactors = F))
# View(data)

# drop countries with unavailable data
data <- data[!(iso3c %in% c("COM","STP","SYC","PSE")),]

# adjust the variables
data[, c("PopTot","GDPpc") := .(log(PopTot), log(GDPpc))]

# Add square of GDPpc and Inflation
data[, c("GDPpc#GDPpc","Infl#Infl","EducIndex#EducIndex") := .(GDPpc^2, Infl^2, EducIndex^2)]

#
#

# analysis

#
#

# filter only data 2000+
data_f <- data[year %in% c(2000:2014),]

# drop observations for which we do not have the depenedent variale
data_f <- data_f[!is.na(GiniNet), ]

# interpolate rule of law (missing for 2001)
data_f[, RuleofLaw := na.interpolation(RuleofLaw, option = "linear"), by = c("iso3c")]

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
		   "WarYears","RevCoups", # invariant in the sample
		   "Age") # only available in 5-year intervals


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
		   "GiniMarket","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

# how many observations are NAN for individual variables?
# sapply(data_try, function(x){sum(is.na(x))})

data_try <- data_try[complete.cases(data_try), ]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))


dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("year","year_2000","WarYears","RevCoups","Top1share","Top10share") := NULL]

data_dm <- data_dm[complete.cases(data_dm), ]
dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_1y_gini_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_1y_gini_baseline, exact = T)
summary(bma_1y_gini_baseline)

# save the results into file
save(bma_1y_gini_baseline, file = here("Results/bma_1y_gini_baseline.Rdata"))

#
#
#
#
#

# Averages - 3 year

#
#
#
#
#

# Averages over years
data_f <- data[year %in% c(2000:2014), ]
data_f[year %in% c(2000:2002), period := 1]
data_f[year %in% c(2003:2005), period := 2]
data_f[year %in% c(2006:2008), period := 3] 
data_f[year %in% c(2009:2011), period := 4]
data_f[year %in% c(2012:2014), period := 5]

# average observations by period and country
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country","period")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(GiniNet),]

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
		   "GiniMarket","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

data_try <- data_try[complete.cases(data_try), ]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# View(data_dm)

# add back the year column
data_dm[, period := data_try$period]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))


dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","Top10share") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_3y_gini_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_3y_gini_baseline, exact = T)
summary(bma_3y_gini_baseline)

# save the results into file
save(bma_3y_gini_baseline, file = here("Results/bma_3y_gini_baseline.Rdata"))


#
#
#

# Averages - 5 year

#
#

# Averages over years
data_f <- data[year %in% c(2000:2014), ]
data_f[year %in% c(2000:2004), period := 1]
data_f[year %in% c(2005:2009), period := 2]
data_f[year %in% c(2010:2014), period := 3] 

# average observations by period and country
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country","period")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(GiniNet),]

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
		   "GiniMarket","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, period := data_try$period]

# drop invariant countries
data_dm <- data_dm[!(iso3c %in% c("CPV","AGO","BEN","LBN","LBR","OMN","QAT","SAU","ZMB")),]

data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))


dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","Top10share") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_5y_gini_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_5y_gini_baseline, exact = T, std.coefs = T)
summary(bma_5y_gini_baseline)

# save the results into file
save(bma_5y_gini_baseline, file = here("Results/bma_5y_gini_baseline.Rdata"))