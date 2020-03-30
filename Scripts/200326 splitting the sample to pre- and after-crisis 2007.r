# Jan Mares, Finance and Income Inequality, revision for dissertation
# March 2020
# Jan Mares

# Splitting the sample in two cross-sections
# pre-crisis / post-crisis

# Libraries
library(data.table)
library(countrycode)
library(dilutBMS2)
library(imputeTS)
library(dummies)

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

# analysis - splitting into 2 cross-sections

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
data_f <- data_beforecrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
data_f[, c('year') := NULL] # dropo year column

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

# order variables, the dependent must be first
setcolorder(data_pre, c("GiniNet", names(data_pre[,names(data_pre)!="GiniNet"])))

#
#

# run BMA
bma_precrisis_gini_baseline <- bms(data_pre, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_precrisis_gini_baseline, exact = T, std.coefs = T)
summary(bma_precrisis_gini_baseline)

# save the results into file
save(bma_precrisis_gini_baseline, file = here("Results/bma_precrisis_gini_baseline.Rdata"))

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
bma_precrisis_gini_dilut <- dilutBMS2::bms(data_pre, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

coef(bma_precrisis_gini_dilut, exact = T)
summary(bma_precrisis_gini_dilut)

# save the results into file
save(bma_precrisis_gini_dilut, file = here("Results/bma_precrisis_gini_dilut.Rdata"))

#
# Post-crisis
#

# Average observations by country
data_f <- data_aftercrisis[, lapply(.SD, mean(as.numeric(x)), na.rm = T), by = c("iso3c","country")]
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
bma_postcrisis_gini_baseline <- bms(data_post, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                            nmodel=5000, mcmc="bd.int", user.int = F)

coef(bma_postcrisis_gini_baseline, exact = T, std.coefs = T)
summary(bma_postcrisis_gini_baseline)

# save the results into file
save(bma_postcrisis_gini_baseline, file = here("Results/bma_postcrisis_gini_baseline.Rdata"))

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
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

coef(bma_postcrisis_gini_dilut, exact = T)
summary(bma_postcrisis_gini_dilut)

# save the results into file
save(bma_postcrisis_gini_dilut, file = here("Results/bma_postcrisis_gini_dilut.Rdata"))

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
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

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
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

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
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

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

# dilution model prior (accounting for corrlation between regressors)
bma_postcrisis_top1_dilut <- dilutBMS2::bms(data_post, iter=5000000, burn=1000000, mprior = "dilut",
						                   g = "hyper", nmodel=5000, mcmc="bd.int",
                  		                   fixed.reg = dummies, user.int = F)

coef(bma_postcrisis_top1_dilut, exact = T)
summary(bma_postcrisis_top1_dilut)

# save the results into file
save(bma_postcrisis_top1_dilut, file = here("Results/bma_postcrisis_top1_dilut.Rdata"))