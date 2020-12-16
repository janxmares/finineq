# Financial Development and Income Inequality
# Alternative with pre-tax Gini index, 3-y averages
# Jan Mares, 1912019

# Libraries
library(data.table)
library(countrycode)
library(BMS)
library(dilutBMS2)
library(imputeTS)
library(here)
library(dummies)

# read data file
data <- data.table(read.csv(file = here('data_hm.csv'), header = T, stringsAsFactors = F))
# View(data)

# drop countries with unavailable data
data <- data[!(iso3c %in% c("BEN","COM","PSE","STP","SYC","SWZ")),]

# adjust the variables
data[, c("PopTot","GDPpc") := .(log(PopTot), log(GDPpc))]

# Add square of GDPpc and Inflation
data[, c("GDPpc#GDPpc","Infl#Infl","EducIndex#EducIndex") := .(GDPpc^2, Infl^2, EducIndex^2)]

#
#
# Averages - 3 year, Gini index
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
data_f <- data_f[, lapply(.SD, function(x) {mean(as.numeric(x), na.rm = T)}), by = c("iso3c","country","period")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(GiniMarket),]

data_try <- data_f[, c("NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "ExpMetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI ) := NULL]
		#  "WarYears","RevCoups", # invariant in the sample
		#    "GiniMarket","RedistRel", # alternative inequality indicators
		   "GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

data_try <- data_try[complete.cases(data_try), ]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# check
# View(data_try)

# add back the year column
data_dm[, period := data_try$period]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
# setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))
setcolorder(data_dm, c("GiniMarket", names(data_dm[,names(data_dm)!="GiniMarket"])))


dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","Top10share") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_3y_pretaxgini_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_3y_pretaxgini_baseline, exact = T, std.coefs = T)
summary(bma_3y_pretaxgini_baseline)

# save the results into file
save(bma_3y_pretaxgini_baseline, file = here("Results/bma_3y_ginimarket_baseline.Rdata"))