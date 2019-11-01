# minimum/median wages OECD
# Jan Mares
# 24/10/2019

# Libraries
library(data.table)
library(countrycode)
library(stringr)
library(countrycode)
library(rsdmx)
library(ggplot2)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# read the OECD data on ratio btetween minimum wage and median wage
# define the query for min/median ratio
q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MIN2AVE/AUS+BEL+CAN+CHL+CZE+EST+FRA+DEU+GRC+HUN+IRL+ISR+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+TUR+GBR+USA+CRI+ROU+COL.MEAN+MEDIAN/all?startTime=1960&endTime=2018"
oecd_wage <- readSDMX(q, isURL = T)

# transform this to data table
oecd_wage_dt <- as.data.table(oecd_wage)

# structure
str(oecd_wage_dt)
head(oecd_wage_dt)

# reshape the series to wide format
oecd_wratio_wide <- data.table(dcast(oecd_wage_dt, COUNTRY + obsTime ~ SERIES, value.var = "obsValue"))

# rename columns
setnames(oecd_wratio_wide, c("iso3c","year","wage.mintomean.OECD","wage.mintomedian.OECD"))

#
#

# average annual wages from oecd since 1990
q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/AV_AN_WAGE/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA.CPNCU+CNPNCU+USDPPP/all?startTime=1990&endTime=2018"
oecd_wage <- readSDMX(q, isURL = T)

# transform into data table
oecd_wage_dt <- as.data.table(oecd_wage)

# filter only values at constant prices in 2018 USD PPP
# unique(oecd_wage_dt$SERIES) # CPNCU - current prices NCU, CNPNCU - constant prices NCU
oecd_wage_dt <- oecd_wage_dt[SERIES == "USDPPP",]

oecd_meanw <- oecd_wage_dt[, j = .(iso3c = COUNTRY, year = obsTime, meanwage.OECD = obsValue)]

#
#

# #
# # not used as these are in national currencies and not really comparable

# # define the query for statutory minimum wage
# q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MW_CURP/AUS+BEL+CAN+CHL+CZE+EST+FRA+DEU+GRC+HUN+IRL+ISR+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+TUR+GBR+USA+CRI+BRA+MLT+ROU+RUS+COL.H+D+W+M+A/all?startTime=1960&endTime=2018"
# oecd_minwage <- readSDMX(q, isURL = T)

# # transform in data table
# oecd_minwage_dt <- as.data.table(oecd_minwage)

# # structure
# str(oecd_minwage_dt)
# head(oecd_minwage_dt)

# # only filter monthly wages
# oecd_minwage_dt <- oecd_minwage_dt[PAYP == "M", ]

# # reshape to wide format
# oecd_mw_wide <- data.table(dcast(oecd_minwage_dt, COUNTRY + obsTime ~ PAYP, value.var = "obsValue"))
# head(oecd_mw_wide)

# # only select annual minimum wage equivalent
# oecd_nmw_f <- oecd_mw_wide[, j = .(iso3c = COUNTRY, year = obsTime, nminwage = A)]

#
#

# real minimum wages (deflated by CPI and converted fo PPP 2018 USD)
q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/RMW/AUS+BEL+CAN+CHL+CZE+EST+FRA+DEU+GRC+HUN+IRL+ISR+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+TUR+GBR+USA+CRI+BRA+RUS+COL.PPP.A/all?startTime=1960&endTime=2018"
oecd_rminwage <- readSDMX(q, isURL = T)

# transfrom into data table
oecd_rminwage_dt <- as.data.table(oecd_rminwage) 

# structure
# str(oecd_rminwage_dt)
# head(oecd_rminwage_dt)
# nrow(oecd_rminwage_dt)

# filter required columns
oecd_rmw_f <- oecd_rminwage_dt[, j = .(iso3c = COUNTRY, year = obsTime, rminwage.OECD = obsValue)]

#
#

# merge the data series together
data_wage_f <- merge(oecd_wratio_wide, oecd_rmw_f, by = c("iso3c","year"), all = T)
data_wage_f <- merge(data_wage_f, oecd_meanw, by = c("iso3c","year"), all = T)

# check
# View(data_wage_f)
# names(data_wage_f)

# only observations past 1980
data_wage_f <- data_wage_f[year >= 1980, ]

# write data into csv
write.csv(data_wage_f, file = "Auxiliary data/to merge/wages oecd.csv", row.names = F)

#
#

# Experiments + analytics

# fig <- ggplot(data_wage_f[iso3c == "USA",], aes(x = year, y = wage_min_mean, group = iso3c))
# fig + geom_line() + 
# 	  geom_line(data = data_wage_f[iso3c == "USA",], aes(x = year, y = wage_min_median, group = iso3c)) +
# 	  geom_line(data = data_wage_f[iso3c == "USA",], aes(x = year, y = rminwage, group = iso3c))

# data_wage_us <- data_wage_f[iso3c == "USA",]
# data_us_l <- melt(data_wage_us, id.vars = c("iso3c","year"))
# head(data_us_l)

# View(data_us_l)
# fig <- ggplot(data_us_l, aes(x = year, y = value, group = iso3c)) 
# fig + geom_line() + facet_grid(variable ~ ., scales = "free")

