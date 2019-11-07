# Libraries
library('WDI')
library('data.table')
library('countrycode')
library('doBy')

# set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# load Solt's inequality database
load(paste0("Auxiliary data/swiid8_1.rda"))

# aggregate the data from different sources
swiid.summary <- summaryBy(gini_disp + gini_mkt + abs_red + rel_red ~ country + year, data = do.call(rbind, swiid), FUN = c(mean, sd))
names(swiid.summary) <- c("country", "year", "gini_disp", "gini_mkt", "abs_red", "rel_red", "gini_disp_se", "gini_mkt_se", "abs_red_se", "rel_red_se")

# Check
# View(swiid.summary)

# assign iso3 codes on the basis of country names
swiid.summary$iso3c <- countrycode(swiid.summary$country,"country.name","iso3c")

# convert to data table
swiid <- data.table(swiid.summary)

# drop Serbia and Montenegro (unavailable iso3c), filter columns
# drop Soviet Union in 1988 - 1990 where the observation
# for Russia are available 
swiid <- swiid[!(country %in% c("Micronesia","Yugoslavia")),] 
swiid <- swiid[!(year %in% c(1988:1990) & country == "Soviet Union"),]

swiid <- swiid[!is.na(iso3c) & year >= 1980, 
				j = .(iso3c, year, GiniMarket = gini_mkt,
					  GiniNet = gini_disp, RedistAbs = abs_red,
					  RedistRel = rel_red)]
#
#

# write to file
write.csv(swiid, file=paste0("Auxiliary data/to merge/swiid.csv"), row.names = F)
