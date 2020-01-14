# Financial Development and Income Inequality
# Baseline analysis, between estimate
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
names(data)
# adjust the variables
data[, c("PopTot","GDPpc") := .(log(PopTot), log(GDPpc))]

# Add square of GDPpc and Inflation
data[, c("GDPpc#GDPpc","Infl#Infl","EducIndex#EducIndex") := .(GDPpc^2, Infl^2, EducIndex^2)]

#
#
#  Gini index
#
#

# Averages over years
data_f <- data[year %in% c(2000:2014), ]

# average observations by country (across time)
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(GiniNet),]

data_try <- data_f[, c("country","iso3c","NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "ExpMetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI ) := NULL]
		#  "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","RedistRel", # alternative inequality indicators
		#    "GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

data_try <- data_try[complete.cases(data_try), ]

# order variables, the dependent must be first
# setcolorder(data_try, c("GiniMarket", names(data_try[,names(data_try)!="GiniMarket"])))
setcolorder(data_try, c("GiniNet", names(data_try[,names(data_try)!="GiniNet"])))

# drop redundant columns, consistency with the panel estimation
data_try[, c("WarYears","RevCoups","Top1share","Top10share") := NULL]

# run BMA
bma_gini_BE <- bms(data_try, iter=500000, burn=100000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_gini_BE, exact = T, std.coefs = T)
summary(bma_gini_BE)

# save the results into file
save(bma_gini_BE, file = here("Results/bma_gini_BE.Rdata"))

#
#

# dilution model prior (accounting for correlation between regressors)
bma_gini_BE_dilut <- dilutBMS2::bms(data_try, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_gini_BE_dilut, exact = T, std.coefs = T)
summary(bma_gini_BE_dilut)

# save the results into file
save(bma_gini_BE_dilut, file = here("Results/bma_gini_BE_dilut.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_gini_BE_random <- bms(data_try, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_gini_BE_random, exact = T, std.coefs = T)
summary(bma_gini_BE_random)

# save the results into file
save(bma_gini_BE_random, file = here("Results/bma_gini_BE_random.Rdata"))

#
#
# Top 10% share
#
#


# Averages over years
data_f <- data[year %in% c(2000:2014), ]

# average observations by country (across time)
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(Top10share),]

data_try <- data_f[, c("country","iso3c","NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "ExpMetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI ) := NULL]
		#  "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","RedistRel", # alternative inequality indicators
		#    "GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

data_try <- data_try[complete.cases(data_try), ]

# order variables, the dependent must be first
setcolorder(data_try, c("Top10share", names(data_try[,names(data_try)!="Top10share"])))

# drop redundant columns, consistency with the panel estimation
data_try[, c("WarYears","RevCoups","Top1share","GiniNet") := NULL]

# run BMA
bma_top10_BE <- bms(data_try, iter=500000, burn=100000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_top10_BE, exact = T, std.coefs = T)
summary(bma_top10_BE)

# save the results into file
save(bma_top10_BE, file = here("Results/bma_top10_BE.Rdata"))

#
#

# dilution model prior (accounting for correlation between regressors)
bma_top10_BE_dilut <- dilutBMS2::bms(data_try, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_top10_BE_dilut, exact = T, std.coefs = T)
summary(bma_top10_BE_dilut)

# save the results into file
save(bma_top10_BE_dilut, file = here("Results/bma_top10_BE_dilut.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_top10_BE_random <- bms(data_try, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_top10_BE_random, exact = T, std.coefs = T)
summary(bma_top10_BE_random)

# save the results into file
save(bma_top10_BE_random, file = here("Results/bma_top10_BE_random.Rdata"))

#
#
# Top 1% share
#
#


# Averages over years
data_f <- data[year %in% c(2000:2014), ]

# average observations by country (across time)
data_f <- data_f[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL]

data_f <- data_f[!is.nan(Top1share),]

data_try <- data_f[, c("country","iso3c","NetFDIout","TaxR","NNSavings","EmplRate","PubEducExp","EnrolPri", # unavailable for a lot of years
		   "EnrolSec","EnrolTer","wage.mintomean.OECD","wage.mintomedian.OECD",  # unavailable for a lot of years
		   "rminwage.OECD","meanwage.OECD","minwage.ILO","meanwage.ILO", "ud", # unavailable for a lot of years
		   "FMA","FME", # unavailable for a lot of years
		   "BankAcc","BankBranches","StockOutTop10","PrivateCredit","MarketCap", # original financial indicators
		   "MarketTurn","ZScore","StockVol","NIM", # original financial indicators
		   "ExpMetalOre","ExpFuel", # correlated with NatRes
		   "corr","burreau","demAcc","lawOrder", # ICRG data
		   "GFCF", # duplicit with NonequipI + EquipI ) := NULL]
		#  "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","RedistRel", # alternative inequality indicators
		#    "GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

data_try <- data_try[complete.cases(data_try), ]

# order variables, the dependent must be first
setcolorder(data_try, c("Top1share", names(data_try[,names(data_try)!="Top1share"])))

# drop redundant columns, consistency with the panel estimation
data_try[, c("WarYears","RevCoups","Top10share","GiniNet") := NULL]

# run BMA
bma_top1_BE <- bms(data_try, iter=500000, burn=100000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_top1_BE, exact = T, std.coefs = T)
summary(bma_top1_BE)

# save the results into file
save(bma_top1_BE, file = here("Results/bma_top1_BE.Rdata"))

#
#

# dilution model prior (accounting for correlation between regressors)
bma_top1_BE_dilut <- dilutBMS2::bms(data_try, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_top1_BE_dilut, exact = T, std.coefs = T)
summary(bma_top1_BE_dilut)

# save the results into file
save(bma_top1_BE_dilut, file = here("Results/bma_top1_BE_dilut.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_top1_BE_random <- bms(data_try, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_top1_BE_random, exact = T, std.coefs = T)
summary(bma_top1_BE_random)

# save the results into file
save(bma_top1_BE_random, file = here("Results/bma_top1_BE_random.Rdata"))