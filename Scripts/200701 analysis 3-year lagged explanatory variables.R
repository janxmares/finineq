# Financial Development and Income Inequality
# Baseline analysis, 3-y averages
# Jan Mares, 1912019

# Libraries
library(data.table)
library(countrycode)
library(BMS)
library(dilutBMS2)
library(imputeTS)
library(here)
library(dummies)
library(xtable)
library(rlang)
library(dplyr)

# SD function

xtsum <- function(data, varname, unit) {
  varname <- enquo(varname)
  loc.unit <- enquo(unit)
ores <- data %>% summarise(ovr.mean=mean(!! varname, na.rm=TRUE), ovr.sd=sd(!! varname, na.rm=TRUE), ovr.min = min(!! varname, na.rm=TRUE), ovr.max=max(!! varname, na.rm=TRUE), ovr.N=sum(as.numeric((!is.na(!! varname)))))
bmeans <- data %>% group_by(!! loc.unit) %>% summarise(meanx=mean(!! varname, na.rm=T), t.count=sum(as.numeric(!is.na(!! varname))))
bres <- bmeans %>% ungroup() %>% summarise(between.sd = sd(meanx, na.rm=TRUE), between.min = min(meanx, na.rm=TRUE), between.max=max(meanx, na.rm=TRUE), Units=sum(as.numeric(!is.na(t.count))), t.bar=mean(t.count, na.rm=TRUE))
wdat <- data %>% group_by(!! loc.unit) %>% mutate(W.x = scale(!! varname, scale=FALSE))
wres <- wdat %>% ungroup() %>% summarise(within.sd=sd(W.x, na.rm=TRUE), within.min=min(W.x, na.rm=TRUE), within.max=max(W.x, na.rm=TRUE))
return(list(ores=ores,bres=bres,wres=wres))
}

# read data file
data <- data.table(read.csv(file = here('data_hm.csv'), header = T, stringsAsFactors = F))
#  View(data)

# drop countries with unavailable data
# data <- data[!(iso3c %in% c("BEN","COM","PSE","STP","SYC","SWZ")),]
data <- data[!(iso3c %in% c("BEN","COM","GIN","LBR","OMN","PSE","STP","SYC","SWZ")),] # excluding also 1 unit observations

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
		#  "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","RedistRel", # alternative inequality indicators
		#    "GiniNet","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu","Age") := NULL] 

# lagged values of explanatory variables
data_lag <- data_try[, lapply(.SD, function(x) {lagp(x, k=1)}), by = c('iso3c')]
data_lag[, GiniNet := NULL]
data_lag <- cbind(data_lag, GiniNet = data_try$GiniNet)

# data_try[, FID_l1 := lagp(FID, k=1), by = c('iso3c')]
# data_try[, FIA_l1 := lagp(FIA, k=1), by = c('iso3c')]
# data_try[, FIE_l1 := lagp(FIE, k=1), by = c('iso3c')]
# data_try[, FMD_l1 := lagp(FMD, k=1), by = c('iso3c')]

# keep only the complete cases
data_try <- data_lag[complete.cases(data_lag), ]
# data_try <- data_try[complete.cases(data_try), ]

# drop the original indicators
# data_try[, c('FIA','FID') := NULL]

#
#
# Summary statistics
#
#

# Filter the variables for correlations
# sumstats <- data_try[, .(GiniNet,Top10share,Top1share,FIA,FIE,FID,FMD)]
# summary(sumstats)

# sds_FIA <- xtsum(data_try, FIA, iso3c)
# sds_FID <- xtsum(data_try, FID, iso3c)
# sds_FIE <- xtsum(data_try, FIE, iso3c)
# sds_FMD <- xtsum(data_try, FMD, iso3c)

# sds_FIA
# sds_FID
# sds_FIE
# sds_FMD

# sd(sumstats$GiniNet)
# sd(sumstats$Top10share)
# sd(sumstats$Top1share)
# sd(sumstats$FIA)
# sd(sumstats$FIE)
# sd(sumstats$FID)
# sd(sumstats$FMD)

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

#
#
#
#
# Correlations
#
#
#
#

# Filter the variables for correlations
# data_corr <- data_dm[, .(GiniNet,Top10share,Top1share,FIA,FIE,FID,FMD)]

# compute the correlation
# corr <- cor(data_corr)
# corr <- round(corr,2) # round to 2 decimal places
# corr[upper.tri(corr)] <- "" # remove the upper triangular part of correlation matrix

# print output to latex
# print(xtable(corr))

#
#
# Estimates
#
#

# check
# View(data_try)

# add back the year column
data_dm[, period := data_try$period]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
# setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))
setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))

dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","Top10share") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_3y_gini_baseline_lag <- bms(data_dm, iter=3000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_3y_gini_baseline_lag, exact = T, std.coefs = T)
summary(bma_3y_gini_baseline_lag)

# save the results into file
save(bma_3y_gini_baseline_lag, file = here("Results/bma_3y_gini_baseline_lag.Rdata"))
load(here("Results/bma_3y_gini_baseline_lag.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_3y_gini_dilut <- dilutBMS2::bms(data_dm, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_gini_dilut_lag, exact = T, std.coefs = T)
summary(bma_3y_gini_dilut_lag)

# save the results into file
save(bma_3y_gini_dilut_lag, file = here("Results/bma_3y_gini_dilut_lag.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_3y_gini_random_lag <- bms(data_dm, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_gini_random_lag, exact = T, std.coefs = T)
summary(bma_3y_gini_random_lag)

# save the results into file
save(bma_3y_gini_random_lag, file = here("Results/bma_3y_gini_random_lag.Rdata"))

#
#
# Top 10% share
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

data_f <- data_f[!is.nan(Top10share),]

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

# lagged values of explanatory variables
data_lag <- data_try[, lapply(.SD, function(x) {lagp(x, k=1)}), by = c('iso3c')]
data_lag[, Top10share := NULL]
data_lag <- cbind(data_lag, Top10share = data_try$Top10share)

# data_try[, FID_l1 := lagp(FID, k=1), by = c('iso3c')]
# data_try[, FIA_l1 := lagp(FIA, k=1), by = c('iso3c')]
# data_try[, FIE_l1 := lagp(FIE, k=1), by = c('iso3c')]
# data_try[, FMD_l1 := lagp(FMD, k=1), by = c('iso3c')]

# keep only the complete cases
data_try <- data_lag[complete.cases(data_lag), ]
# data_try <- data_try[complete.cases(data_try), ]

# drop the original indicators
# data_try[, c('FIA','FID') := NULL]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# check
# View(data_dm)

# add back the year column
data_dm[, period := data_try$period]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("Top10share", names(data_dm[,names(data_dm)!="Top10share"])))


dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","GiniNet") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_3y_top10_baseline_lag <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_3y_top10_baseline_lag, exact = T)
summary(bma_3y_top10_baseline_lag)

# save the results into file
save(bma_3y_top10_baseline_lag, file = here("Results/bma_3y_top10_baseline_lag.Rdata"))
load(here("Results/bma_3y_top10_baseline_lag.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_3y_top10_dilut_lag <- dilutBMS2::bms(data_dm, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_top10_dilut_lag, exact = T)
summary(bma_3y_top10_dilut_lag)

# save the results into file
save(bma_3y_top10_dilut_lag, file = here("Results/bma_3y_top10_dilut_lag.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_3y_top10_random_lag <- bms(data_dm, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
				          g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_top10_random_lag, exact = T)
summary(bma_3y_top10_random_lag)

# save the results into file
save(bma_3y_top10_random_lag, file = here("Results/bma_3y_top10_random_lag.Rdata"))

#
#
# Top 1% share
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

data_f <- data_f[!is.nan(Top1share),]

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

# lagged values of explanatory variables
data_lag <- data_try[, lapply(.SD, function(x) {lagp(x, k=1)}), by = c('iso3c')]
data_lag[, Top1share := NULL]
data_lag <- cbind(data_lag, Top1share = data_try$Top1share)

# data_try[, FID_l1 := lagp(FID, k=1), by = c('iso3c')]
# data_try[, FIA_l1 := lagp(FIA, k=1), by = c('iso3c')]
# data_try[, FIE_l1 := lagp(FIE, k=1), by = c('iso3c')]
# data_try[, FMD_l1 := lagp(FMD, k=1), by = c('iso3c')]

# keep only the complete cases
data_try <- data_lag[complete.cases(data_lag), ]
# data_try <- data_try[complete.cases(data_try), ]

# drop the original indicators
# data_try[, c('FIA','FID') := NULL]

#
#

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# check
# View(data_dm)

# add back the year column
data_dm[, period := data_try$period]
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("Top1share", names(data_dm[,names(data_dm)!="Top1share"])))


dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top10share","GiniNet") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

# run BMA
bma_3y_top1_baseline_lag <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_3y_top1_baseline_lag, exact = T)
summary(bma_3y_top1_baseline_lag)

# save the results into file
save(bma_3y_top1_baseline_lag, file = here("Results/bma_3y_top1_baseline_lag.Rdata"))
load(here("Results/bma_3y_top1_baseline_lag.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_3y_top1_dilut_lag <- dilutBMS2::bms(data_dm, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_top1_dilut_lag, exact = T)
summary(bma_3y_top1_dilut_lag)

# save the results into file
save(bma_3y_top1_dilut_lag, file = here("Results/bma_3y_top1_dilut_lag.Rdata"))
                                    
#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_3y_top1_random_lag <- bms(data_dm, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_3y_top1_random_lag, exact = T)
summary(bma_3y_top1_random_lag)
          
# save the results into file
save(bma_3y_top1_random_lag, file = here("Results/bma_3y_top1_random_lag.Rdata"))