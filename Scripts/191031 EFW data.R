# Read economic freedom index, fraser institute
# Jan Mares, 191031

# Libraries
library(data.table)
library(countrycode)
library(XLConnect)
library(stringr)


# Set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)

# education index
# load worksheet
efw_wb <- loadWorkbook("Auxiliary data/efw2017paneldata.xlsx")
efw <- data.table(readWorksheet(efw_wb, sheet="Sheet1", startRow=1, startCol=1), stringsAsFactors=F)

# adjust column names
setnames(efw, c("Countries","Size.of.Government","Legal.System...Property.Rights",
				"Sound.Money","Freedom.to.trade.internationally","Regulation"),
			  c("country","GovSize","LegalSystem","SoundMoney","TradeFreedom","Regulation"))

# drop country column
efw[, country := NULL]

# only observations past 1980
efw <- efw[year >= 1980, ]

#
#

# write data into file
write.csv(efw, file = "Auxiliary data/to merge/efw2017paneldata.csv", row.names = F)