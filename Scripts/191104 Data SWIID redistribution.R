# Libraries
library('WDI')
library('data.table')
library('countrycode')
library('doBy')

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# Load Solt's inequality database
load(paste0("Auxiliary data/swiid8_1.rda"))

# aggregate the data from different sources
swiid.summary <- summaryBy(gini_disp + gini_mkt + abs_red + rel_red ~ country + year, data = do.call(rbind, swiid), FUN = c(mean, sd))
names(swiid.summary) <- c("country", "year", "gini_disp", "gini_mkt", "abs_red", "rel_red", "gini_disp_se", "gini_mkt_se", "abs_red_se", "rel_red_se")

# Check
# View(swiid.summary)

# assign iso3 codes on the basis of country names
swiid.summary$iso3c <- countrycode(swiid.summary$country,"country.name","iso3c")

# convert to data table
redist <- data.table(swiid.summary)

# drop Serbia and Montenegro (unavailable iso3c)
redist <- redist[!is.na(iso3c),]

# filter columns

head(redist)
redist_avg <- redist[year %in% c(1980:2009),.(Redist=mean(abs_red), Redist_relative=mean(rel_red)), by=c("iso2c")]

#
#

write.csv(gini_avg, file=paste0(datadir,"redist swiid 1980-2009.csv"), row.names=F)
