# Read data on control variables from PWT
# Jan Mares, 191030

# Libraries
library(data.table)
library(countrycode)
library(pwt9)
library(readstata13)

# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

#
#

# Load Penn World table 
pwt <- data.table(read.csv(file="Auxiliary data/pwt91.csv", header=T, stringsAsFactors=F))

# Data on capital
capital_detail <- data.table(read.dta13("Auxiliary data/pwt91_capital_detail.dta"))

# Restrict to only 1980+
capital_detail <- capital_detail[year %in% c(1980:2017),]

# Construct the constant prices series
capital_detail[,Ir_Mach:=Ic_Mach/Ip_Mach]
capital_detail[,Ir_Struc:=Ic_Struc/Ip_Struc]
capital_detail[,Ir_TraEq:=Ic_TraEq/Ip_TraEq]
capital_detail[,Ir_Other:=Ic_Other/Ip_Other]

# Construct the equipment and non-equipment shares within the fixed capital formation
capital_detail[,mach_share:=Ir_Mach/(Ir_Struc+Ir_TraEq+Ir_Other+Ir_Mach)]
capital_detail[,rest_share:=(Ir_Struc+Ir_TraEq+Ir_Other)/(Ir_Struc+Ir_TraEq+Ir_Other+Ir_Mach)]

#
#

# Get fixed capital formation shares from basic pwt
gfcf <- pwt[year %in% c(1980:2017),j=list(iso3c = countrycode, country = country, year = year, csh_i = csh_i)]

# Merge fixed capital formation with detail on capital
investment_f <- merge(gfcf, capital_detail[,j = list(iso3c = countrycode,year = year, mach_share = mach_share, rest_share = rest_share)],
					  by = c("iso3c","year"))

# Construct the shares of equip and nonequip investment
investment_f[, EquipI := mach_share*csh_i]
investment_f[, NonequipI := rest_share*csh_i]
investment_f[, to:= mach_share + rest_share]

# select required columns
investment_f <- investment_f[, j = .(iso3c, year, EquipI, NonequipI)]

# write into csv
write.csv(investment_f, file = "Auxiliary data/to merge/investment shares detail.csv", row.names = F)