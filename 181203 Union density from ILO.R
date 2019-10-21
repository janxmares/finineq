# Union density from ILO
# Jan Mares
# 3/12/2018

# Analysis of WID data on inequality
# Jan Mares, 181108

# Libraries
library(data.table)
library(countrycode)
library(Rilostat)


# Set the working directory
wd <- c("c:/Users/JM/Dropbox/Research/Finance and income inequality/")
setwd(wd)

a <- get_ilostat_toc(search="union")

data_ilo_ud <- data.table(get_ilostat("ILR_TUMT_NOC_RT_A"))
data_ilo_minwage <- data.table(get_ilostat("EAR_4MNP_NOC_NB_A"))
data_ilo_meanwage <- data.table(get_ilostat("EAR_4MTH_SEX_ECO_CUR_NB_A"))


head(data_ilo_meanwage)

data_ilo_ud[ref_area=="CZE",]


unique(data_ilo_ud$ref_area)


View(data_ilo_meanwage)