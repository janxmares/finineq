# Read data on control variables from WB
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(WDI)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#
# # List of series from WB
# 
# NY.GDP.TOTL.RT.ZS # 		Total natural resources rents
# NE.CON.GOVT.ZS # 			General government final consumption expenditure (% of GDP)
# NY.GDS.TOTL.ZS # 			Gross domestic savings (% of GDP)
# SP.POP.GROW # 			Annual population growth
# FP.CPI.TOTL.ZG # 			Inflation, consumer prices (annual %)
# NY.ADJ.AEDU.GN.ZS # 		Adjusted savings: education expenditure (% of GNI)
# NV.IND.TOTL.KD.ZG # 		Value added in industry (% GDP)
# NV.AGR.TOTL.ZS # 			Value added in agriculture (% GDP)
# BX.KLT.DINV.WD.GD.Z # 	Foreign direct investment, net inflows (% of GDP)
# BM.KLT.DINV.WD.GD.ZS #	Foreign direct investment, net outflows (% of GDP)
# GC.TAX.TOTL.GD.ZS # 		Tax revenue (% of GDP)
# NY.ADJ.NNAT.GN.ZS # 		Adjusted savings: net national savings (% of GNI)
# RL.EST #					Rule of Law: Estimate
# SP.DYN.IMRT.IN # 			Mortality rate, infant (per 1,000 live births)
# NE.GDI.FTOT.ZS # 			Gross fixed capital formation (% of GDP)
# SL.EMP.TOTL.SP.NE.ZS # 	Employment to population ratio, 15+, total (%) (national estimate)
# SE.XPD.TOTL.GD.ZS # 		Public education expenditures as a share of GDP
# NY.GDP.PCAP.PP.KD #		GDP per capita, PPP (constant 2011 international $)
# NE.TRD.GNFS.ZS #          Trade (% of GDP)
# SP.DYN.LE00.IN # 			Life expectancy at birth
# TX.VAL.MMTL.ZS.UN #		Ores and metals exports (% of merchandise exports)
# TX.VAL.FUEL.ZS.UN # 		Fuel exports (% of merchandise exports)

#
#

# NOT USED FOR NOW

# IC.REG.DURS #				Time required to start a business (days)
# IC.BUS.NDNS.ZS # 			New business density (new registrations per 1,000 people ages 15-64)
# IC.REG.COST.PC.ZS # 		Cost of business start-up procedures (% of GNI per capita)
# GC.REV.XGRT.GD.ZS # 		Revenue, excluding grants (% of GDP)
# HD.HCI.HLOS #				Harmonized Test Scores # ONLY AVAILABLE IN 2018	
# SL.EMP.TOTL.SP.ZS #		Employment to population ratio, 15+, total (%) (modeled ILO estimate)
# NY.GDP.MKTP.CD # 			Gross domestic product (GDP), current market prices $US

#
#

# Load data from WB
wbvars <- c("NY.GDP.TOTL.RT.ZS","NE.CON.GOVT.ZS","NY.GDS.TOTL.ZS","SP.POP.GROW",
			"FP.CPI.TOTL.ZG","NY.ADJ.AEDU.GN.ZS","NV.IND.TOTL.ZS","NV.AGR.TOTL.ZS",
			"BX.KLT.DINV.WD.GD.ZS","BM.KLT.DINV.WD.GD.ZS","GC.TAX.TOTL.GD.ZS",
			"NY.ADJ.NNAT.GN.ZS","RL.EST","SP.DYN.IMRT.IN","NE.GDI.FTOT.ZS",
			"SL.EMP.TOTL.SP.NE.ZS","SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.PP.KD",
			"NE.TRD.GNFS.ZS","SP.DYN.LE00.IN","TX.VAL.MMTL.ZS.UN")

# Read first dataset to obtain selected country codes
wbdata <- data.table(WDI(indicator=wbvars, start=1990, end=2017))

# Selection of countries to drop from the analysis
codedrop <- c("1A","1W","4E","7E","8S","B8","EU","F1","V1","V2","V3","V4","T2","T3","T4","T5","T6","T7",
			  "AN","FM","FO","IM","JG","KI","KM","KN","KV","KY","LC","MF","MH","MP","OE","PF","PS","S1",
			  "S2","S3","S4","SB","SC","SM","ST","SV","SX","TC","TL","TV","VC","VI","XC","XD","XE",
			  "XF","XG","XH","XI","XJ","XL","XL","XM","XN","XO","XP","XQ","XR","XS","XT","XK","XU",
			  "XY","Z4","Z7","ZF","ZG","ZJ","ZQ","ZT","VG","NR","","..")

# Define selected country codes
selcodes <- unique(wbdata$iso2c[!(wbdata$iso2c %in% codedrop)])

# Total natural resources rents
wbdatasub <- wbdata[iso2c %in% selcodes,]

# Assign iso3c code to the datas
wbdatasub[,iso3c:=countrycode(wbdatasub$iso2c,"iso2c", "iso3c")]

# rename the variables
setnames(wbdatasub, c("NY.GDP.TOTL.RT.ZS","NE.CON.GOVT.ZS","NY.GDS.TOTL.ZS",
				      "SP.POP.GROW","FP.CPI.TOTL.ZG","NY.ADJ.AEDU.GN.ZS",
				      "NV.IND.TOTL.ZS","NV.AGR.TOTL.ZS","BX.KLT.DINV.WD.GD.ZS",
				      "BM.KLT.DINV.WD.GD.ZS","GC.TAX.TOTL.GD.ZS","NY.ADJ.NNAT.GN.ZS",
				      "RL.EST","SP.DYN.IMRT.IN","NE.GDI.FTOT.ZS","SL.EMP.TOTL.SP.NE.ZS",
				      "SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.PP.KD","NE.TRD.GNFS.ZS",
				      "SP.DYN.LE00.IN","TX.VAL.MMTL.ZS.UN","TX.VAL.FUEL.ZS.UN"),
					c("NatRes","GovExp","GDSavings","PopGrowth","Infl","EducExp","VAI",
					  "VAA","NetFDIin","NetFDIout","TaxR","NNSavings","RuleofLaw","Mortality","GFCF",
					  "EmplRate","PubEducExp","GDPpc","TradeOpen","LifeExp","ExpMetalOre",
					  "ExpFuel"))

# Check
# names(wbdatasub)
# # View(wbdatasub)

#
#

# write data
write.csv(wbdatasub, file = "Auxiliary data/wb data.csv")