# Union density from ILO
# Jan Mares
# 3/12/2018
# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)
library(Rilostat)
library(ggplot2)


# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

srch <- get_ilostat_toc(search="union")

# download the series froim ILOSTAT
data.ilo.ud <- data.table(get_ilostat("ILR_TUMT_NOC_RT_A"))

#
#

# filter required columns + rename
data.ilo.ud <- data.ilo.ud[,.(iso3c = ref_area, year = time, ud = obs_value)]

# only observations past 1980
data.ilo.ud <- data.ilo.ud[year >= 1980, ]

# what countries do we have data for?
# unique(data.ilo.ud$ref_area)

# since when is the data available? earliest observation?
# min(data.ilo.ud$year)

# check
# View(data.ilo.ud)

# how is scandinavia doing?
# data_scand <- data.ilo.ud[iso3c %in% c('SWE','FIN','DNK','NOR'), ]
# fig.scand <- ggplot(data_scand, aes(x=year, y=ud, group=iso3c, colour=iso3c))
# fig.scand + geom_line()

# write data info file
write.csv(data.ilo.ud, file="Auxiliary data/to merge/union density ILO.csv", row.names = F)