# Figures for the paper
# Between estimates, figures
# Jan Mares, 191218

# Libraries
library(data.table)
library(imputeTS)
library(here)
library(ggplot2)
library(gridExtra)

# read data file
data <- data.table(read.csv(file = here('data_hm.csv'), header = T, stringsAsFactors = F))
# View(data)

# drop countries with unavailable data
# data <- data[!(iso3c %in% c("BEN","COM","PSE","STP","SYC","SWZ")),]
data <- data[!(iso3c %in% c("BEN","COM","GIN","LBR","OMN","PSE","STP","SYC","SWZ")),] # excluding also 1 unit observations

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

#
#
#   Figures
#
#

# Set up color scheme
colset <- matrix(c(192,0,0,
                   55,96,146), ncol=3, byrow=T)

# Top10 and top1 share to GiniNet
pl1 <- ggplot(data_try, aes(Top10share, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Top 10% income share')
		      
pl1g <- pl1 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl1g

pl2 <- ggplot(data_try, aes(Top1share, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Top 1% income share')
		      
pl2g <- pl2 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl2g

#
# Scatter plots of the variable with highest PIPs
#

# FIE
pl1 <- ggplot(data_try, aes(FIE, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Financial institutions efficiency')
		      
pl1g <- pl1 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl1g

# EPS file
cairo_ps(file = here("Paper/figures/FIEGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl1g
dev.off()

# FIA
pl2 <- ggplot(data_try, aes(FIA, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Access to financial institutions')
		      
pl2g <- pl2 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl2g

# EPS file
cairo_ps(file = here("Paper/figures/FIAGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl2g
dev.off()

# FID
pl3 <- ggplot(data_try, aes(FID, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Financial institutions depth')
		      
pl3g <- pl3 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl3g

# EPS file
cairo_ps(file = here("Paper/figures/FIDGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl3g
dev.off()

# FMD
pl4 <- ggplot(data_try, aes(FMD, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Financial markets depth')
		      
pl4g <- pl4 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl4g

# EPS file
cairo_ps(file = here("Paper/figures/FMDGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl4g
dev.off()

# GovExp
pl5 <- ggplot(data_try, aes(GovExp, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Government expenditures (% GDP)')
		      
pl5g <- pl5 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl5g

# EPS file
cairo_ps(file = here("Paper/figures/GovExpGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl5g
dev.off()

# Mortality
pl6 <- ggplot(data_try, aes(Mortality, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Mortality')
		      
pl6g <- pl6 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl6g

# EPS file
cairo_ps(file = here("Paper/figures/MortGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl6g
dev.off()

# GDP
pl7 <- ggplot(data_try, aes(GDPpc, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('GDP per capita')
		      
pl7g <- pl7 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl7g

# EPS file
cairo_ps(file = here("Paper/figures/GDPGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl7g
dev.off()

# RuleofLaw
pl8 <- ggplot(data_try, aes(RuleofLaw, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Rule of law')
		      
pl8g <- pl8 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl8g

# EPS file
cairo_ps(file = here("Paper/figures/RuleLawGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl8g
dev.off()

# Inflation
pl9 <- ggplot(data_try, aes(Infl, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Inflation')
		      
pl9g <- pl9 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl9g

# EPS file
cairo_ps(file = here("Paper/figures/InflGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl9g
dev.off()

# Equipment investment
pl10 <- ggplot(data_try, aes(EquipI, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Equipment investment')
		      
pl10g <- pl10 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl10g

# EPS file
cairo_ps(file = here("Paper/figures/EquipIGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl10g
dev.off()

# Trade openness
pl11 <- ggplot(data_try, aes(TradeOpen, GiniNet)) +
		      ylab("Gini index - income (after redistribution)") +
		      xlab('Trade openness')
		      
pl11g <- pl11 + geom_point(size=2) +
		theme_bw() +
		theme(plot.margin = unit(c(0.3,0.2,0.3,0.3), "cm"), 
			  panel.border = element_rect(fill = NA, colour = 'black'),
    		  legend.position="none") +
        # stat_smooth(method='lm', se=F) +
        scale_colour_manual(values=rgb(colset/255)) + 
        scale_fill_manual(values=rgb(colset/255))

pl11g

# EPS file
cairo_ps(file = here("Paper/figures/TradeOpenGiniNet_BE.eps"), width=9, height=5, family="Helvetica")
pl11g
dev.off()

#
#
# Grids
#
#

cairo_ps(file = here("Paper/figures/plots_findev_gini_BE.eps"), width=9, height=9, family="Arial")
grid.arrange(pl1g, pl2g, pl3g, pl4g, ncol=2)
dev.off()