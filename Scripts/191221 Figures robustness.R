#  Figures for the paper, top 10 share
# Jan Mares, 191221

# Libraries
library(data.table)
library(dilutBMS2)
library(here)
library(ggplot2)
library(gridExtra)

# plot comparison, different model priors

# Gini 

# load the files with results
load(here("Results/bma_3y_gini_dilut.Rdata"))
load(here("Results/bma_3y_gini_baseline.Rdata"))
load(here("Results/bma_3y_gini_random.Rdata"))

#
#

pl_comp_gini <- plotComp(bma_3y_gini_baseline, bma_3y_gini_dilut, bma_3y_gini_random,
                         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
                         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
                         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

# write in file
cairo_ps(file = here("Paper/figures/model_priors_comparison_gini.eps"), width=9, height=6, family="Arial")
par(mar=c(6,4,0,0))
plotComp(bma_3y_gini_baseline, bma_3y_gini_dilut, bma_3y_gini_random,
         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend("topright", legend=c("Uniform", "Dilution","Random"), pch=c(1:3), bty="n", inset=c(0.02,0.02),
       title = expression(bold("Model priors")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()

# plot comparison - Top 10 share
load(here("Results/bma_3y_top10_dilut.Rdata"))
load(here("Results/bma_3y_top10_baseline.Rdata"))
load(here("Results/bma_3y_top10_random.Rdata"))

plotComp(bma_3y_top10_baseline, bma_3y_top10_dilut, bma_3y_top10_random,
         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

# write in file
cairo_ps(file = here("Paper/figures/model_priors_comparison_top10.eps"), width=9, height=6, family="Arial")
par(mar=c(6,4,0,0))
plotComp(bma_3y_top10_baseline, bma_3y_top10_dilut, bma_3y_top10_random,
         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend("topright", legend=c("Uniform", "Dilution","Random"), pch=c(1:3), bty="n", inset=c(0.02,0.02),
       title = expression(bold("Model priors")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

dev.off()


# Top 1 share
load(here("Results/bma_3y_top1_dilut.Rdata"))
load(here("Results/bma_3y_top1_baseline.Rdata"))
load(here("Results/bma_3y_top1_random.Rdata"))

plotComp(bma_3y_top1_baseline, bma_3y_top1_dilut, bma_3y_top1_random,
         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

# write in file
cairo_ps(file = here("Paper/figures/model_priors_comparison_top1.eps"), width=9, height=6, family="Arial")
par(mar=c(6,4,0,0))
plotComp(bma_3y_top1_baseline, bma_3y_top1_dilut, bma_3y_top1_random,
         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend("topright", legend=c("Uniform", "Dilution","Random"), pch=c(1:3), bty="n", inset=c(0.02,0.02),
       title = expression(bold("Model priors")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

dev.off()

#
#
#
#
#

# comparison of different inequality measures

# variables to plot (PIP > 0.9 in at least one specification)
varplot_baseline <- c("EducExp","Unemployment","NonequipI","FIA","EducIndex",
                      "RedistAbs","LifeExp","EFW","VAA","GovExp","PopTot","Infl","EquipI",
                      "FMD","TradeOpen","RuleofLaw","NatRes","FID","GDPpc")

varplot_dilut <- c("EducExp","Unemployment","FIA","Redist","PopTot",
                   "VAA","NonequipI","GovExp","Infl","EquipI",
                   "FMD", "LifeExp", "TradeOpen", "RuleofLaw","NatRes",
                   "FID")#,"GDPpc")

# comparison, inequality measures, top shares, all variables
par(mar=c(6,4,0,0))
plotComp(bma_3y_top10_baseline, bma_3y_top1_baseline,
                         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
                         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
                         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend("topright", legend=c("Gini index", "Top 10% share","Top 1% share"), pch=c(1:3), bty="n", inset=c(0.02,0.02),
       title = expression(bold("Models")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

#
#

# write into file
cairo_ps(file = here("Paper/figures/topsh_comparison_all.eps"), width=9, height=6, family="Arial")
par(mar=c(6,4,0,0))
plotComp(bma_3y_top10_baseline, bma_3y_top1_baseline,
                         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = F,
                         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
                         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend("topright", legend=c("Top 10% share","Top 1% share"), pch=c(1:3), bty="n", inset=c(0.02,0.02),
       title = expression(bold("Models")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

dev.off()

#
#

# comparison, inequality measures, top variables PIP
par(mar=c(10,4,0,0))
plotComp(bma_3y_gini_baseline, bma_3y_top10_baseline, bma_3y_top1_baseline,
         lwd=1, varNr = varplot_baseline, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=0.8, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Gini index","Top 10% share","Top 1% share"), pch=c(1:3), bty="n", inset=c(0,-0.25),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
#
#
# Write into file
cairo_ps(file = here("Paper/figures/comp_baseline_pip_sel.eps"), width=9, height=9, family="Arial")
par(mar=c(10,4,0,0))
plotComp(bma_3y_gini_baseline, bma_3y_top10_baseline, bma_3y_top1_baseline,
         lwd=1, varNr = varplot_baseline, comp = "PIP", exact = T, include.legend = F,
         add.grid = F, do.par = F, cex.axis=1, cex.xaxis=1,
         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753, 0, 0),rgb(0, 0, 0)))

legend("bottom", legend = c("Gini index","Top 10% share","Top 1% share"), pch=c(1:3), bty="n", inset=c(0,-0.25),
                          title = expression(bold("Models")), title.adj = 0.5, xpd = T, horiz=T,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))
dev.off()

#
#
#
#
#

# comparison, inequality measures, top variables Std Means
pl_comp_mean_topsh <- plotComp(bma_3y_gini_baseline, bma_3y_top10_baseline, bma_3y_top1_baseline,
                         lwd=1, varNr = varplot_baseline, comp = "Std Mean", exact = T, include.legend = F,
                         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.8,
                         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

legend(x=8.5,y=0.95, legend=c("Gini index","Top 10% share","Top 1% share"), pch=c(1:3), bty="n",
       title = expression(bold("Models")), title.adj = 0.1, col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

#
#