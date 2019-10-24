# iso2 codes from WID world
# Jan Mares
# 21/10/2019

# Libraries
library(data.table)
library(countrycode)
library(stringr)
library(countrycode)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# read data
iso2 <- data.table(read.csv("Auxiliary data/WID iso2codes.csv", header = F, stringsAsFactors=F))

# correct the first csv line
iso2[1, V1:="AD Andorra"]

# set column name
setnames(iso2, c("country"))

# split by the first space and bind into data table
l1 <- str_replace(iso2$country, "[:space:]{1}", "_")
l1 <- str_split(l1, "_")
l1_dt <- data.table(do.call(rbind, l1))

# set the data table column names
setnames(l1_dt, c("iso2c","country"))

# check
# View(l1_dt)

# write into csv
write.csv(l1_dt, file="Auxiliary data/WID iso2codes.csv", row.names = F)