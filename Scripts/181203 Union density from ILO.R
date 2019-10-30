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
data_ilo_ud <- data.table(get_ilostat("ILR_TUMT_NOC_RT_A"))

#
#

# filter required columns + rename
data_ilo_ud <- data_ilo_ud[,.(iso3c = ref_area, year = time, ud = obs_value)]

# what countries do we have data for?
unique(data_ilo_ud$ref_area)
# since when is the data available? earliest observation?
min(data_ilo_ud$year)

# check
# View(data_ilo_ud)

# how is scandinavia doing?
data_scand <- data_ilo_ud[iso3c %in% c('SWE','FIN','DNK','NOR'), ]
fig.scand <- ggplot(data_scand, aes(x=year, y=ud, group=iso3c, colour=iso3c))
fig.scand + geom_line()

# write data info file
write.csv(data_ilo_ud, file="Auxiliary data/union density ILO.csv", row.names = F)