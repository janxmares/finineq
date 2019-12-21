# Read data on control variables from WB
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(rsdmx)
library(fredr)

# set APi key for FRED
fredr_set_key("7c540f109056100978e41ecdbdec8d59")

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
# SP.POP.TOTL # 			Population, total
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
# NY.GDP.MKTP.KD.ZG			GDP, annual growth (%)
# NE.TRD.GNFS.ZS #          Trade (% of GDP)
# SP.DYN.LE00.IN # 			Life expectancy at birth
# TX.VAL.MMTL.ZS.UN #		Ores and metals exports (% of merchandise exports)
# TX.VAL.FUEL.ZS.UN # 		Fuel exports (% of merchandise exports)
# SE.PRM.ENRR #				School enrollment, primary
# SE.SEC.ENRR #				School enrollment, secondary
# SE.TER.ENRR #				School enrollment, tertiary
# SL.UEM.TOTL.ZS 			Unemployment, total (% of total labor force) (ILO estimate)

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
# SL.UEM.TOTL.NE.ZS			Unemployment, total (% of total labor force) (national estimate)

#
#

# Load data from WB
wbvars <- c("NY.GDP.TOTL.RT.ZS","NE.CON.GOVT.ZS","NY.GDS.TOTL.ZS","SP.POP.GROW","SP.POP.TOTL",
			"FP.CPI.TOTL.ZG","NY.ADJ.AEDU.GN.ZS","NV.IND.TOTL.ZS","NV.AGR.TOTL.ZS",
			"BX.KLT.DINV.WD.GD.ZS","BM.KLT.DINV.WD.GD.ZS","GC.TAX.TOTL.GD.ZS",
			"NY.ADJ.NNAT.GN.ZS","RL.EST","NE.GDI.FTOT.ZS",
			"SL.EMP.TOTL.SP.NE.ZS","SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.PP.KD",
			"NY.GDP.MKTP.KD.ZG","NE.TRD.GNFS.ZS","SP.DYN.LE00.IN",
			"TX.VAL.MMTL.ZS.UN","TX.VAL.FUEL.ZS.UN","SE.PRM.ENRR",
			"SE.SEC.ENRR","SE.TER.ENRR","SL.UEM.TOTL.ZS")

# Read first dataset to obtain selected country codes
wbdata <- data.table(WDI(indicator = wbvars, start = 1980, end = 2017))

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
wbdatasub[, iso3c := countrycode(wbdatasub$iso2c,"iso2c", "iso3c")]
wbdatasub[, iso2c := NULL]

# rename the variables
setnames(wbdatasub, c("NY.GDP.TOTL.RT.ZS","NE.CON.GOVT.ZS","NY.GDS.TOTL.ZS",
				      "SP.POP.GROW","SP.POP.TOTL","FP.CPI.TOTL.ZG","NY.ADJ.AEDU.GN.ZS",
				      "NV.IND.TOTL.ZS","NV.AGR.TOTL.ZS","BX.KLT.DINV.WD.GD.ZS",
				      "BM.KLT.DINV.WD.GD.ZS","GC.TAX.TOTL.GD.ZS","NY.ADJ.NNAT.GN.ZS",
				      "RL.EST","NE.GDI.FTOT.ZS","SL.EMP.TOTL.SP.NE.ZS",
				      "SE.XPD.TOTL.GD.ZS","NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG",
					  "NE.TRD.GNFS.ZS","SP.DYN.LE00.IN","TX.VAL.MMTL.ZS.UN",
					  "TX.VAL.FUEL.ZS.UN","SE.PRM.ENRR","SE.SEC.ENRR","SE.TER.ENRR",
					  "SL.UEM.TOTL.ZS"),
					c("NatRes","GovExp","GDSavings","PopGrowth","PopTot","Infl","EducExp","VAI",
					  "VAA","NetFDIin","NetFDIout","TaxR","NNSavings","RuleofLaw","GFCF",
					  "EmplRate","PubEducExp","GDPpc","GDPgrowth","TradeOpen","LifeExp","ExpMetalOre",
					  "ExpFuel","EnrolPri","EnrolSec","EnrolTer","Unemployment"))

# Check
# names(wbdatasub)
# View(wbdatasub)

# Add inflation data for Argentina as they are not available from WB
# From FRED
infl <- as.data.table(fredr_series_observations(
  series_id = "FPCPITOTLZGARG",
  observation_start = as.Date("1980-01-01"),
  frequency = "a",
  units = c("lin")
))

infl[, iso3c := 'ARG']
infl[, year := year(date)]
infl <- infl[, j = .(iso3c, year, InflArg = value)]

# merge wb data and fred on Argentina
wbdatasub <- merge(wbdatasub, infl, by = c("iso3c","year"), all = T)
wbdatasub[iso3c == "ARG" & is.na(Infl), Infl := InflArg]
wbdatasub[, InflArg := NULL]

# Add inflation data for Mozambique as they are not available from WB
infl <- as.data.table(fredr_series_observations(
  series_id = "FPCPITOTLZGMOZ",
  observation_start = as.Date("1980-01-01"),
  frequency = "a",
  units = c("lin")
))

infl[, iso3c := 'MOZ']
infl[, year := year(date)]
infl <- infl[, j = .(iso3c, year, InflMoz = value)]

# merge wb data and fred on Mozambique
wbdatasub <- merge(wbdatasub, infl, by = c("iso3c","year"), all = T)
wbdatasub[iso3c == "MOZ" & is.na(Infl), Infl := InflMoz]
wbdatasub[, InflMoz := NULL]

#
#

# Add inflation data for Cyprusn as they are not available from WB
# from Eurostat
source("Scripts/read.eurostat.R")
infl_eurostat <- read.eurostat('prc_hicp_aind', flags = F)

# filter year on year changes for cyprus, overall HICP
infl_cyp <- infl_eurostat[UNIT == "RCH_A_AVG" & GEO == "CY" & COICOP == "CP00",]

# reshape to long format, adjust years, assign iso3 code
infl_cyp[, c("UNIT","COICOP") := NULL]
infl_cyp_l <- melt(infl_cyp, id.vars = c("GEO"), variable.name = "year", value.name = "InflCyp")

infl_cyp_l[, year := as.numeric(str_replace(year, "X",""))]

infl_cyp_l[, iso3c := countrycode(GEO, "iso2c", "iso3c")]
infl_cyp_l[, c("GEO") := NULL]

# merge with main data
wbdatasub <- merge(wbdatasub, infl_cyp_l, by = c("iso3c","year"), all = T)
wbdatasub[iso3c == "CYP" & is.na(Infl), Infl := InflCyp]
wbdatasub[, InflCyp := NULL]

#
#

# View(wbdatasub)

#
#

# write data
write.csv(wbdatasub, file = "Auxiliary data/to merge/wb data.csv", row.names = F)