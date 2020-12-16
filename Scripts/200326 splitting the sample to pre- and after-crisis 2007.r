# Jan Mares, Finance and Income Inequality, revision for dissertation
# March 2020
# Jan Mares

# Splitting the sample in two cross-sections
# pre-crisis / post-crisis

# Libraries
library(data.table)
library(countrycode)
# library(dilutBMS2)
library(imputeTS)
library(dummies)
library(BMS)
# Set the working directory
library(here)

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

# analysis - full sample averaged

#
#

# Estimate using full sample
data_full <- data

# Average observations by country
data_f <- data_full[, lapply(.SD, function(x) {mean(as.numeric(x), na.rm = T)}), by = c("iso3c","country")]
data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!is.nan(GiniNet),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_full <- data_try[, c("iso3c","country","WarYears","RevCoups","Top1share","Top10share") := NULL]

# order variables, the dependent must be first
setcolorder(data_full, c("GiniNet", names(data_pre[,names(data_pre)!="GiniNet"])))

#
#

# run BMA
bma_full_gini_baseline <- bms(data_full, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_full_gini_baseline, exact = T, std.coefs = T)
summary(bma_full_gini_baseline)

# save the results into file
save(bma_full_gini_baseline, file = here("Results/bma_full_gini_baseline.Rdata"))
# load(file = here("Results/bma_full_gini_baseline.Rdata"))

#
#

# analysis - full sample averaged

#
#

# Split the sample into 2 periods
data_beforecrisis <- data[year %in% c(2000:2007), ]
data_aftercrisis <- data[year %in% c(2008:2014), ]

# 
#
# After-tax income Gini coefficient
#
#

#
# Pre-crisis
#

# Average observations by country
data_f <- data_beforecrisis[, lapply(.SD, function(x) {mean(as.numeric(x), na.rm = T)}), by = c("iso3c","country")]
data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!is.nan(GiniNet),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_pre <- data_try[, c("iso3c","country","WarYears","RevCoups","Top1share","Top10share") := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# order variables, the dependent must be first
setcolorder(data_pre, c("GiniNet", names(data_pre[,names(data_pre)!="GiniNet"])))

#
#

# run BMA
bma_precrisis_gini_baseline <- bms(data_pre, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_gini_baseline, exact = T, std.coefs = T)
BMS::summary(bma_precrisis_gini_baseline)

# save the results into file
save(bma_precrisis_gini_baseline, file = here("Results/bma_precrisis_gini_baseline.Rdata"))
load(file = here("Results/bma_precrisis_gini_baseline.Rdata"))

#
#

bma_precrisis_gini_random <- bms(data_pre, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_gini_random, exact = T, std.coefs = T)
summary(bma_precrisis_gini_random)

# save the results into file
save(bma_precrisis_gini_random, file = here("Results/bma_precrisis_gini_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_precrisis_gini_dilut <- dilutBMS2::bms(data_pre, iter=3000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_gini_dilut, exact = T)
summary(bma_precrisis_gini_dilut)

# save the results into file
save(bma_precrisis_gini_dilut, file = here("Results/bma_precrisis_gini_dilut.Rdata"))
load(file = here("Results/bma_precrisis_gini_dilut.Rdata"))

#
# Post-crisis
#

# Average observations by country
data_f <- data_aftercrisis[, lapply(.SD, function(x) {mean(as.numeric(x), na.rm = T)}), by = c("iso3c","country")]
data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!is.nan(GiniNet),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_post <- data_try[, c("iso3c","country","WarYears","RevCoups","Top1share","Top10share") := NULL]

# order variables, the dependent must be first
setcolorder(data_post, c("GiniNet", names(data_post[,names(data_post)!="GiniNet"])))

#
#


# run BMA
bma_postcrisis_gini_baseline <- bms(data_post, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_gini_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_gini_baseline)

# save the results into file
save(bma_postcrisis_gini_baseline, file = here("Results/bma_postcrisis_gini_baseline.Rdata"))
load(here("Results/bma_postcrisis_gini_baseline.Rdata"))

#
#

bma_postcrisis_gini_random <- bms(data_post, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_gini_random, exact = T, std.coefs = T)
summary(bma_postcrisis_gini_random)

# save the results into file
save(bma_postcrisis_gini_random, file = here("Results/bma_postcrisis_gini_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_postcrisis_gini_dilut <- dilutBMS2::bms(data_post, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_gini_dilut, exact = T)
summary(bma_postcrisis_gini_dilut)

# save the results into file
save(bma_postcrisis_gini_dilut, file = here("Results/bma_postcrisis_gini_dilut.Rdata"))
load(file = here("Results/bma_postcrisis_gini_dilut.Rdata"))

#
#
# Top 10% income share
#
#

#
# Pre-crisis
#

# Average observations by country
data_f <- data_beforecrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL] # dropo year column

data_f <- data_f[!is.nan(Top10share),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_pre <- data_try[, c("iso3c","country","WarYears","RevCoups","GiniNet","Top1share") := NULL]

# order variables, the dependent must be first
setcolorder(data_pre, c("Top10share", names(data_pre[,names(data_pre)!="Top10share"])))

#
#

# run BMA
bma_precrisis_top10_baseline <- bms(data_pre, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top10_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_top10_baseline)

# save the results into file
save(bma_precrisis_top10_baseline, file = here("Results/bma_precrisis_top10_baseline.Rdata"))
# load(file = here("Results/bma_precrisis_top10_baseline.Rdata"))

#
#

bma_precrisis_top10_random <- bms(data_pre, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top10_random, exact = T, std.coefs = T)
summary(bma_precrisis_top10_random)

# save the results into file
save(bma_precrisis_top10_random, file = here("Results/bma_precrisis_top10_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_precrisis_top10_dilut <- dilutBMS2::bms(data_pre, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top10_dilut, exact = T)
summary(bma_precrisis_top10_dilut)

# save the results into file
save(bma_precrisis_top10_dilut, file = here("Results/bma_precrisis_top10_dilut.Rdata"))

#
# Post-crisis
#

# Average observations by country
data_f <- data_aftercrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!is.nan(Top10share),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_post <- data_try[, c("iso3c","country","WarYears","RevCoups","GiniNet","Top1share") := NULL]

# order variables, the dependent must be first
setcolorder(data_post, c("Top10share", names(data_post[,names(data_post)!="Top10share"])))

#
#


# run BMA
bma_postcrisis_top10_baseline <- bms(data_post, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top10_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_top10_baseline)

# save the results into file
save(bma_postcrisis_top10_baseline, file = here("Results/bma_postcrisis_top10_baseline.Rdata"))
# load(file = here("Results/bma_postcrisis_top10_baseline.Rdata"))

#
#

bma_postcrisis_top10_random <- bms(data_post, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top10_random, exact = T, std.coefs = T)
summary(bma_postcrisis_top10_random)

# save the results into file
save(bma_postcrisis_top10_random, file = here("Results/bma_postcrisis_top10_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_postcrisis_top10_dilut <- dilutBMS2::bms(data_post, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top10_dilut, exact = T)
summary(bma_postcrisis_top10_dilut)

# save the results into file
save(bma_postcrisis_top10_dilut, file = here("Results/bma_postcrisis_top10_dilut.Rdata"))

#
#
# Top 1% income share
#
#

#
# Pre-crisis
#

# Average observations by country
data_f <- data_beforecrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL] # dropo year column

data_f <- data_f[!is.nan(Top1share),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_pre <- data_try[, c("iso3c","country","WarYears","RevCoups","GiniNet","Top1share") := NULL]

# order variables, the dependent must be first
setcolorder(data_pre, c("Top1share", names(data_pre[,names(data_pre)!="Top1share"])))

#
#

# run BMA
bma_precrisis_top1_baseline <- bms(data_pre, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top1_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_top1_baseline)

# save the results into file
save(bma_precrisis_top1_baseline, file = here("Results/bma_precrisis_top1_baseline.Rdata"))
# load(file = here("Results/bma_precrisis_top1_baseline.Rdata"))

#
#

bma_precrisis_top1_random <- bms(data_pre, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top1_random, exact = T, std.coefs = T)
summary(bma_precrisis_top1_random)

# save the results into file
save(bma_precrisis_top1_random, file = here("Results/bma_precrisis_top1_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_precrisis_top1_dilut <- dilutBMS2::bms(data_pre, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_top1_dilut, exact = T)
summary(bma_precrisis_top1_dilut)

# save the results into file
save(bma_precrisis_top1_dilut, file = here("Results/bma_precrisis_top1_dilut.Rdata"))

#
# Post-crisis
#

# Average observations by country
data_f <- data_aftercrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!is.nan(Top1share),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# drop redundant columns
data_post <- data_try[, c("iso3c","country","WarYears","RevCoups","GiniNet","Top1share") := NULL]

# order variables, the dependent must be first
setcolorder(data_post, c("Top1share", names(data_post[,names(data_post)!="Top1share"])))

#
#


# run BMA
bma_postcrisis_top1_baseline <- bms(data_post, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top1_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_top1_baseline)

# save the results into file
save(bma_postcrisis_top1_baseline, file = here("Results/bma_postcrisis_top1_baseline.Rdata"))
load(file = here("Results/bma_postcrisis_top1_baseline.Rdata"))

#
#

bma_postcrisis_top1_random <- bms(data_post, iter=5000000, burn=1000000, mprior = "random", mprior.size = 7,
				          g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top1_random, exact = T, std.coefs = T)
summary(bma_postcrisis_top1_random)

# save the results into file
save(bma_postcrisis_top1_random, file = here("Results/bma_postcrisis_top1_random.Rdata"))

#
#

# dilution model prior (accounting for correlation between regressors)
bma_postcrisis_top1_dilut <- dilutBMS2::bms(data_post, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_top1_dilut, exact = T)
summary(bma_postcrisis_top1_dilut)

# save the results into file
save(bma_postcrisis_top1_dilut, file = here("Results/bma_postcrisis_top1_dilut.Rdata"))
load(file = here("Results/bma_postcrisis_top1_dilut.Rdata"))

#
#

# comparison pre/post-crisis
plotComp(bma_precrisis_gini_dilut, bma_postcrisis_gini_dilut)

# 
#
# Attempt to capture before and after crisis regime with yearly observation
#
#

#
# Gini coefficient

#
# Pre-crisis
#

# Average observations by country
data_beforecrisis <- data[year %in% c(2000:2007), ]
data_f <- data_beforecrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(GiniNet) | is.na(GiniNet)),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_pre <- data_dm[, c("year","year_2000","iso3c","country","WarYears","RevCoups","Top1share","Top10share") := NULL]


# order variables, the dependent must be first
setcolorder(data_pre, c("GiniNet", names(data_pre[,names(data_pre)!="GiniNet"])))
data_pre <- data_pre[complete.cases(data_pre), ]

#
#

# run BMA
bma_precrisis_1y_gini_baseline <- bms(data_pre, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, fixed.reg = dummies, mcmc="bd.int", user.int = F)

coef(bma_precrisis_1y_gini_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_1y_gini_baseline)

# save the results into file
save(bma_precrisis_1y_gini_baseline, file = here("Results/bma_precrisis_1y_gini_baseline.Rdata"))
load(file = here("Results/bma_precrisis_1y_gini_baseline.Rdata"))

#
#
# post-crisis
#
#

# Average observations by country
data_aftercrisis <- data[year %in% c(2008:2014), ]
data_f <- data_aftercrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(GiniNet) | is.na(GiniNet)),] # any NA in Gini?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]

#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_post <- data_dm[, c("year","year_2008","iso3c","country","WarYears","RevCoups","Top1share","Top10share") := NULL]


# order variables, the dependent must be first
setcolorder(data_post, c("GiniNet", names(data_post[,names(data_post)!="GiniNet"])))
data_post <- data_post[complete.cases(data_post), ]

#
#

# run BMA
bma_postcrisis_1y_gini_baseline <- bms(data_post, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_1y_gini_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_1y_gini_baseline)

# save the results into file
save(bma_postcrisis_1y_gini_baseline, file = here("Results/bma_postcrisis_1y_gini_baseline.Rdata"))
load(file = here("Results/bma_postcrisis_1y_gini_baseline.Rdata"))

# comparison pre/post-crisis
plotComp(bma_precrisis_1y_gini_baseline, bma_postcrisis_1y_gini_baseline)

# Plot the comparison of pre- and post- crisis
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_gini_baseline, bma_postcrisis_1y_gini_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.5),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

# write the figure into file



#
#

#
# Top 10 share

#
# Pre-crisis
#

# Average observations by country
data_beforecrisis <- data[year %in% c(2000:2007), ]
data_f <- data_beforecrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(Top10share) | is.na(Top10share)),] # any NA in topshare?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,1:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_pre <- data_dm[, c("year","year_2000","year_2001","iso3c","country","WarYears","RevCoups","Top1share","GiniNet") := NULL]

# order variables, the dependent must be first
setcolorder(data_pre, c("Top10share", names(data_pre[,names(data_pre)!="Top10share"])))
data_pre <- data_pre[complete.cases(data_pre), ]

#
#

# run BMA
bma_precrisis_1y_top10_baseline <- bms(data_pre, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_1y_top10_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_1y_top10_baseline)

# save the results into file
save(bma_precrisis_1y_top10_baseline, file = here("Results/bma_precrisis_1y_top10_baseline.Rdata"))
load(file = here("Results/bma_precrisis_1y_top10_baseline.Rdata"))

#
#
# post-crisis
#
#

# Average observations by country
data_aftercrisis <- data[year %in% c(2008:2014), ]
data_f <- data_aftercrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(Top10share) | is.na(Top10share)),] # any NA in topshare?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,1:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_post <- data_dm[, c("year","year_2008","iso3c","country","WarYears","RevCoups","Top1share","GiniNet") := NULL]


# order variables, the dependent must be first
setcolorder(data_post, c("Top10share", names(data_post[,names(data_post)!="Top10share"])))
data_post <- data_post[complete.cases(data_post), ]

#
#

# run BMA
bma_postcrisis_1y_top10_baseline <- bms(data_post, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_1y_top10_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_1y_top10_baseline)

# save the results into file
save(bma_postcrisis_1y_top10_baseline, file = here("Results/bma_postcrisis_1y_top10_baseline.Rdata"))
load(file = here("Results/bma_postcrisis_1y_top10_baseline.Rdata"))

# Plot the comparison of pre- and post- crisis
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_top10_baseline, bma_postcrisis_1y_top10_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.5),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
						  
#
#

#
#

#
# Top 1 share

#
# Pre-crisis
#

# Average observations by country
data_beforecrisis <- data[year %in% c(2000:2007), ]
data_f <- data_beforecrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(Top1share) | is.na(Top1share)),] # any NA in topshare?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,1:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_pre <- data_dm[, c("year","year_2000","year_2001","iso3c","country","WarYears","RevCoups","Top10share","GiniNet") := NULL]

# order variables, the dependent must be first
setcolorder(data_pre, c("Top1share", names(data_pre[,names(data_pre)!="Top1share"])))
data_pre <- data_pre[complete.cases(data_pre), ]

#
#

# run BMA
bma_precrisis_1y_top1_baseline <- bms(data_pre, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_1y_top1_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_1y_top1_baseline)

# save the results into file
save(bma_precrisis_1y_top1_baseline, file = here("Results/bma_precrisis_1y_top1_baseline.Rdata"))
load(file = here("Results/bma_precrisis_1y_top1_baseline.Rdata"))

#
#
# post-crisis
#
#

# Average observations by country
data_aftercrisis <- data[year %in% c(2008:2014), ]
data_f <- data_aftercrisis
# data_f[, c('year') := NULL] # drop year column

data_f <- data_f[!(is.nan(Top1share) | is.na(Top1share)),] # any NA in topshare?

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
		   "PrEdu","SecEdu","TerEdu",
		   'Age') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,1:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, year := data_try$year]
#
#

dum <- dummy(data_dm$year, sep = "_")
data_dm <- cbind(data_dm, dum)
dummies <- colnames(dum)[2:(ncol(dum))]

# drop redundant columns
data_post <- data_dm[, c("year","year_2008","iso3c","country","WarYears","RevCoups","Top10share","GiniNet") := NULL]


# order variables, the dependent must be first
setcolorder(data_post, c("Top1share", names(data_post[,names(data_post)!="Top1share"])))
data_post <- data_post[complete.cases(data_post), ]

#
#

# run BMA
bma_postcrisis_1y_top1_baseline <- bms(data_post, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_1y_top1_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_1y_top1_baseline)

# save the results into file
save(bma_postcrisis_1y_top1_baseline, file = here("Results/bma_postcrisis_1y_top1_baseline.Rdata"))
load(file = here("Results/bma_postcrisis_1y_top1_baseline.Rdata"))

#
#

# Plot the comparison of pre- and post- crisis
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_top1_baseline, bma_postcrisis_1y_top1_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.5),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
#
#

# write figures into files

#
#

# gini
cairo_ps(file = here("figures/comp_prepostcrisis_gini.eps"), width=8, height=6, family="Arial")
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_gini_baseline, bma_postcrisis_1y_gini_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.65),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()

# top 10
cairo_ps(file = here("figures/comp_prepostcrisis_top10.eps"), width=8, height=6, family="Arial")
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_top10_baseline, bma_postcrisis_1y_top10_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.55),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()						  

# top 1
cairo_ps(file = here("figures/comp_prepostcrisis_top1.eps"), width=8, height=6, family="Arial")
par(mar=c(12,4,0,0))
plotComp(bma_precrisis_1y_top1_baseline, bma_postcrisis_1y_top1_baseline,
         lwd=1, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Pre-crisis","Post-crisis"), pch=c(1:3), bty="n", inset=c(0,-0.6),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()