# 5-year averages, market gini coefficient
# Jan Mares, 200326

# Libraries
library(data.table)
library(countrycode)
library(dilutBMS2)
library(imputeTS)
library(here)
library(dummies)

# # Set the working directory
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

# analysis - Averages - 5 year

#
#

# Averages over years
data_f <- data[year %in% c(2000:2014), ]
data_f[year %in% c(2000:2004), period := 1]
data_f[year %in% c(2005:2009), period := 2]
data_f[year %in% c(2010:2014), period := 3] 

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
		   # "WarYears","RevCoups", # invariant in the sample
		   "GiniMarket","RedistRel", # alternative inequality indicators
		   "GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation", # subcomponents of EFW
		   "PrEdu","SecEdu","TerEdu") := NULL]

data_try <- data_try[complete.cases(data_try), ]

# demean the data
data_dm <- data_try[, lapply(.SD[,2:ncol(.SD)], demean, na.rm = T), by = c("iso3c","country")]

# add back the year column
data_dm[, period := data_try$period]

# drop invariant countries
data_dm <- data_dm[!(iso3c %in% c("AGO","BEN","CPV","ETH","GIN","LBN","LBR","OMN","QAT","SAU","ZMB")),]

# drop redundant columns
data_dm[, c("iso3c","country") := NULL]

# order variables, the dependent must be first
setcolorder(data_dm, c("GiniNet", names(data_dm[,names(data_dm)!="GiniNet"])))

# create the period dummies
dum <- dummy(data_dm$period, sep = "_")
data_dm <- cbind(data_dm, dum)
data_dm[, c("period","period_1","WarYears","RevCoups","Top1share","Top10share") := NULL]

dummies <- colnames(dum)[2:(ncol(dum))]

#
#

# run BMA
bma_5y_gini_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_5y_gini_baseline, exact = T, std.coefs = T)
summary(bma_5ygini_baseline)

# save the results into file
save(bma_5y_gini_baseline, file = here("Results/bma_5y_gini_baseline.Rdata"))
load('Results/bma_5y_gini_baseline.Rdata')

#
#

bma_5y_gini_random <- bms(data_dm, iter=1000000, burn=500000, mprior = "random", mprior.size = 25,
				          g = 17, nmodel=5000, mcmc="bd.int",
	                      fixed.reg = dummies, user.int = F)

plotModelsize(bma_5y_gini_random)

coef(bma_5y_gini_random, exact = T)
summary(bma_5y_gini_random)
gdensity(bma_5y_gini_random)

# save the results into file
save(bma_5y_gini_random, file = here("Results/bma_5y_gini_random.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_5y_gini_dilut <- dilutBMS2::bms(data_dm, iter=1000000, burn=500000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_5y_gini_dilut, exact = T)
summary(bma_5y_gini_dilut)

# save the results into file
save(bma_5y_gini_dilut, file = here("Results/bma_5y_gini_dilut.Rdata"))

#
#
# Top 10% share
#
#


# Averages over years
data_f <- data[year %in% c(2000:2014), ]
data_f[year %in% c(2000:2004), period := 1]
data_f[year %in% c(2005:2009), period := 2]
data_f[year %in% c(2010:2014), period := 3] 

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

data_try <- data_try[complete.cases(data_try), ]

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
bma_5y_top10_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_5y_top10_baseline, exact = T)
summary(bma_5y_top10_baseline)

# save the results into file
save(bma_5y_top10_baseline, file = here("Results/bma_5y_top10_baseline.Rdata"))

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_5y_top10_dilut <- dilutBMS2::bms(data_dm, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_5y_top10_dilut, exact = T)
summary(bma_5y_top10_dilut)

# save the results into file
save(bma_5y_top10_dilut, file = here("Results/bma_5y_top10_dilut.Rdata"))

#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_5y_top10_random <- bms(data_dm, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
				          g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_5y_top10_random, exact = T)
summary(bma_5y_top10_random)

# save the results into file
save(bma_5y_top10_random, file = here("Results/bma_5y_top10_random.Rdata"))

#
#
# Top 1% share
#
#


# Averages over years
data_f <- data[year %in% c(2000:2014), ]
data_f[year %in% c(2000:2004), period := 1]
data_f[year %in% c(2005:2009), period := 2]
data_f[year %in% c(2010:2014), period := 3] 

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

data_try <- data_try[complete.cases(data_try), ]

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
bma_5y_top1_baseline <- bms(data_dm, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                    nmodel=5000, mcmc="bd.int",
                    fixed.reg = dummies, user.int = F)

coef(bma_5y_top1_baseline, exact = T)
summary(bma_5y_top1_baseline)

# save the results into file
save(bma_5y_top1_baseline, file = here("Results/bma_5y_top1_baseline.Rdata"))
load("Results/bma_5y_top1_baseline.Rdata")

#
#

# dilution model prior (accounting for corrlation between regressors)
bma_5y_top1_dilut <- dilutBMS2::bms(data_dm, iter=5000000, burn=1000000, mprior = "dilut",
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_5y_top1_dilut, exact = T)
summary(bma_5y_top1_dilut)

# save the results into file
save(bma_5y_top1_dilut, file = here("Results/bma_5y_top1_dilut.Rdata"))
                                    
#
#

# random model prior (accounting for a number of regressors in prior model probability)
bma_5y_top1_random <- bms(data_dm, iter=5000000, burn=1000000, mprior = "random", mprior.size = 5,
						  g = "hyper", nmodel=5000, mcmc="bd.int",
                  		  fixed.reg = dummies, user.int = F)

coef(bma_5y_top1_random, exact = T)
summary(bma_5y_top1_random)
          
# save the results into file
save(bma_5y_top1_random, file = here("Results/bma_5y_top1_random.Rdata"))

#
#
#
#
#

# load the baseline 3y results
load(here("Results/bma_3y_gini_baseline.Rdata"))
load(here("Results/bma_3y_top10_baseline.Rdata"))
load(here("Results/bma_3y_top1_baseline.Rdata"))

# plot variable setup
comp_vars_gini <- row.names(coef(bma_3y_gini_baseline, exact = T))
comp_vars_gini <- comp_vars_gini[!(comp_vars_gini %in% c('period_4','period_5'))]
comp_vars_top10 <- row.names(coef(bma_3y_top10_baseline, exact = T))
comp_vars_top10 <- comp_vars_top10[!(comp_vars_top10 %in% c('period_4','period_5'))]
comp_vars_top1 <- row.names(coef(bma_3y_top1_baseline, exact = T))
comp_vars_top1 <- comp_vars_top1[!(comp_vars_top1 %in% c('period_4','period_5'))]

# Comparison with 3y averages, gini
par(mar=c(12,4,0,0))
plotComp(bma_3y_gini_baseline, bma_5y_gini_baseline,
		 varNr = comp_vars_gini, lwd=1, comp = "PIP", exact = T, 
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.55),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

#
#

# Comparison with 3y averages, top 10 share
par(mar=c(12,4,0,0))
plotComp(bma_3y_top10_baseline, bma_5y_top10_baseline,
		 varNr = comp_vars_top10, lwd=1, comp = "PIP", exact = T, 
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.6),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

#
#

# Comparison with 3y averages, top 1 share
par(mar=c(11,4,0,0))
plotComp(bma_3y_top1_baseline, bma_5y_top1_baseline,
		 varNr = comp_vars_top1, lwd=1, comp = "PIP", exact = T,
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.55),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

#
#

# # Write into file, gini
cairo_ps(file = here("figures/comp_3y5y_gini.eps"), width=8, height=6, family="Arial")
# Comparison with 3y averages, gini
par(mar=c(12,4,0,0))
plotComp(bma_3y_gini_baseline, bma_5y_gini_baseline,
		 varNr = comp_vars_gini, lwd=1, comp = "PIP", exact = T, 
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.55),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()

# # Write into file, top 10 share
cairo_ps(file = here("figures/comp_3y5y_top10.eps"), width=8, height=6, family="Arial")
par(mar=c(12,4,0,0))
plotComp(bma_3y_top10_baseline, bma_5y_top10_baseline,
		 varNr = comp_vars_top10, lwd=1, comp = "PIP", exact = T, 
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.6),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()

# # Write into file, top 1 share
cairo_ps(file = here("figures/comp_3y5y_top1.eps"), width=8, height=6, family="Arial")
par(mar=c(11,4,0,0))
plotComp(bma_3y_top1_baseline, bma_5y_top1_baseline,
		 varNr = comp_vars_top1, lwd=1, comp = "PIP", exact = T,
		 include.legend = F, add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("3-year baseline","5-year baseline"), pch=c(1:2), bty="n", inset=c(0,-0.55),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()