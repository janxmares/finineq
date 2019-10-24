# minimum/median wages OECD
# Jan Mares
# 24/10/2019

# Libraries
library(data.table)
library(countrycode)
library(stringr)
library(countrycode)
library(rsdmx)

# Set the working directory
wd <- c("c:/Users/JM/Dropbox/Research/Finance and income inequality/")
setwd(wd)

# read the OECD data on ratio btetween minimum wage and median wage
# define the query for min/median ratio
q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MIN2AVE/AUS+BEL+CAN+CHL+CZE+EST+FRA+DEU+GRC+HUN+IRL+ISR+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+TUR+GBR+USA+CRI+ROU+COL.MEAN+MEDIAN/all?startTime=1960&endTime=2018"
oecd_wage <- readSDMX(q, isURL = T)

# transform this to data table
oecd_wage_dt <- as.data.table(oecd_wage)

#
#

# define the query for statutory minimum wage
q <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MW_CURP/AUS+BEL+CAN+CHL+CZE+EST+FRA+DEU+GRC+HUN+IRL+ISR+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+TUR+GBR+USA+CRI+BRA+MLT+ROU+RUS+COL.H+D+W+M+A/all?startTime=1960&endTime=2018"
oecd_minwage <- readSDMX(q, isURL = T)

# transform in data table
oecd_minwage_dt <- as.data.table(oecd_minwage)

str(oecd_minwage_dt)
head(oecd_minwage_dt)