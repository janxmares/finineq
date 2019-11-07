# Read KOF globalization data
# Jan Mares, 191104

# Libraries
library('WDI')
library('data.table')
library('countrycode')

# set the working directory
wd <- c("c:/Users/JM/Documents/GitHub/finineq/")
setwd(wd)


# KOF globalization indices
glob <- data.table(read.csv("Auxiliary data/KOFGlobalization_Data_2018.csv", header = T, stringsAsFactors = F))

# Only select variables accounting for de jure economic globalization, and political + social globalization
# Then average across time span
glob <- glob[year %in% c(1980:2017), j=.(year, globFin = KOFFiGIdf,
									 	 globRestrict = KOFEcGIdj,
									 	 globSoc = KOFSoGI,
									 	 globPol = KOFPoGI), by = c("code")]

setnames(glob, c("code"),c("iso3c"))

# Drop regions and aggregated areas
drops <- c("WLD","EAS","ECS","LCN","MEA","NAC","SAS","SSF","HIC","LIC","LMC","UMC")
glob <- glob[!(iso3c %in% drops),]

# check
# head(glob)
# View(glob)
# adjust iso3 code for romania and congo
glob[iso3c == "ROM", iso3c := "ROU"]
glob[iso3c == "ZAR", iso3c := "COD"]

# write into file
write.csv(glob, file = "Auxiliary data/to merge/kof.csv", row.names = F)
