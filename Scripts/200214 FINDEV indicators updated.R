# Read financial development indicators, IMF up to date version
# Jan Mares, 200214

# Libraries
library(data.table)
library(countrycode)
library(WDI)
library(readstata13)
library(XLConnect)
library(here)

# financial indicators
# load worksheet
findev_wb <- loadWorkbook("Auxiliary data/FD Index Database (Excel).xlsx")
findev <- data.table(readWorksheet(findev_wb,sheet="Sheet1", startRow=1, startCol=1), stringsAsFactors=F)

# drop obsolete variables
findev[, c("Dataset.Name","Dataset.Code","Country.Code","imf_region","imf_income","Scale.Name") := NULL]

# rename the data columns
setnames(findev, c("iso3c", "country", "year", "FD", "FI", "FM", "FID", "FIA", "FIE", "FMD", "FMA", "FME"))

# only keep the country data, not large geographical areas
drops <- c("Africa","All countries","AM","Asia and Pacific","EM","Europe","LIC","Middle East and Central Asia","Western Hemisphere")
findev <- findev[!(iso3c %in% drops), ]

# replace 0 values (unobserved) with NAs
findev[FID == 0, FID := NA]
findev[FIA == 0, FIA := NA]
findev[FIE == 0, FIE := NA]
findev[FMD == 0, FMD := NA]
findev[FMA == 0, FMA := NA]
findev[FME == 0, FME := NA]

# check
# head(findev)
# View(findev)

# write into file
write.csv(findev, file = here("Auxiliary data/to merge/findev new.csv"), row.names = F)
save.dta13(findev, file = here("Auxiliary data/to merge/findev new.dta"))