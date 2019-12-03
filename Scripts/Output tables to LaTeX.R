# Output tables to LaTeX
# Jan Mares, 191203

# Libraries
library(data.table)
library(here)
library(xtable)

# Load the results
load(file = here("Results/bma_3y_baseline.Rdata"))

#
#

# Creating ouput tables

# Variable names setup
explvarshort <- c("NatRes","GovExp","GDSavings","PopGrowth","PopTot","Infl","EducExp",
                  "VAI","VAA","NetFDIin","RuleofLaw","Mortality","GDPpc","TradeOpen",
                  "LifeExp","Unemployment","EquipI","NonequipI","kaopen","FID","FIA",
                  "FIE","FMD","EFW","EducIndex","CLandPR","LeftWing","RedistAbs","globFin",
                  "globRestrict","globSoc","globPol","GDPpc#GDPpc","Infl#Infl",
                  "EducIndex#EducIndex","period_2","period_3","period_4","period_5")

explvarnames <- c("Natural resources rents","Government expenditures","Gross domestic savings",
                  "Population growth","Total population","Inflation","Education expenditures",
                  "Value added in industry","Value added in agriculture","Net FDI (% GDP)",
                  "Rule of law","Mortality","GDP per capita", "Trade openness","Life expectancy",
                  "Unemployment","Equipment investment","Non-equipment investment","Chinn-Ito index",
                  "Financial institutions depth","Access to financial institutions","Financial institutions efficiency",
                  "Financial markets depth","Economic freedom","Education index (UN)","Civil liberties & political rights",
                  "Left-wing orientation","Redistribution","Financial globalization","Restrictions on globalization",
                  "Social globalization","Political globalization","GDP per capita^2", "Inflation^2",
                  "Education index^2","Period 2","Period 3","Period 4","Period 5")

# Bind short and long variables names together
varnames <- cbind(explvarshort, explvarnames)  

# Results hyper prior
results_all <- coef(bma_3y_baseline, exact=T)

# List of variables sorted by PIP 
varlist <- data.frame(explvarshort=row.names(results_all))

# Creating concordance table
concordance <- merge(varlist, varnames, by="explvarshort", sort=F)

# Inputt variable names
row.names(results_all) <- concordance$explvarnames

# LaTeX output with proper varnames
xtable(results_all[,1:3], digits=c(0,2,5,5))