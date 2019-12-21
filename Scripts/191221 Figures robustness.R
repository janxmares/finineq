#  Figures for the paper, top 10 share
# Jan Mares, 191221

# Libraries
library(data.table)
library(dilutBMS2)
library(here)
library(ggplot2)

# Gini 

# load the files with results
load(here("Results/bma_3y_gini_dilut.Rdata"))
load(here("Results/bma_3y_gini_baseline.Rdata"))
load(here("Results/bma_3y_gini_random.Rdata"))

pl_comp_gini <- plotComp(bma_3y_gini_baseline, bma_3y_gini_dilut, bma_3y_gini_random,
                         lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = TRUE,
                         add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.65,
                         col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

cairo_ps(file = paste0(folderfig,"model_priors_comparison_gini.eps"), width=9, height=6, family="helvetica")
pl_comp_gini
dev.off()

# Top 10 share
load(here("Results/bma_3y_top10_dilut.Rdata"))
load(here("Results/bma_3y_top10_baseline.Rdata"))
load(here("Results/bma_3y_top10_random.Rdata"))

pl_comp_top10 <- plotComp(bma_3y_top10_baseline, bma_3y_top10_dilut, bma_3y_top10_random,
                          lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = TRUE,
                          add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.65,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

cairo_ps(file = paste0(folderfig,"model_priors_comparison_top10.eps"), width=9, height=6, family="helvetica")
pl_comp_top10
dev.off()


# Top 1 share
load(here("Results/bma_3y_top1_dilut.Rdata"))
load(here("Results/bma_3y_top1_baseline.Rdata"))
load(here("Results/bma_3y_top1_random.Rdata"))

pl_comp_top1 <- plotComp(bma_3y_top1_baseline, bma_3y_top1_dilut, bma_3y_top1_random,
                          lwd=1, varNr = NULL, comp = "PIP", exact = T, include.legend = TRUE,
                          add.grid = F, do.par = T, cex.axis=0.8, cex.xaxis=0.65,
                          col=c(rgb(0.03, 0.15, 0.4),rgb(0.753,0,0),rgb(0, 0, 0)))

cairo_ps(file = paste0(folderfig,"model_priors_comparison_top1.eps"), width=9, height=6, family="helvetica")
pl_comp_top1
dev.off()
