#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0 ) {
  args[1] <- 3 }

library(stats)
library(dplyr)
library(forcats)
library(viridis)
library(ggplot2)
library(ggsignif)
library(readxl)
library(MASS)
library(ggsignif)
library(tidyverse)
library(psych)
library(ggplot2)
library(stats)
library(ggpubr)
library(ggdendro)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(Dict)

source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Pipeline/scripts/r/file_names.R", sep = ""))
pca.df.temporal <- read.csv(paste(ROOT_FOLDER,"/Pipeline/Output/ouput_pca.csv", sep = ""))


alpha <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "alpha",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

old.min <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "old.min",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

uls <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "uls",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

pa  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "pa",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

ml  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1]) ,rotate= "varimax", fm = "ml",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

minrank  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "minrank",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

wls  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "wls",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

cum_list <- Dict$new("alpha" = alpha,
                     "Principal Axisis"  = pa,
                     "Maximum likelihood"  = ml,
                      "uls" = uls,
                       "minimal residual" = old.min,
                      "minimum rank "= minrank)

cum_list <- Dict$new("Principal Axisis"  = pa,
                     "Maximum likelihood"  = ml)

h <- 0
method_name <- ""
method <- ""
for (i in c(1: length(cum_list$keys))){
  if (h < max(cum_list$get(i)$Vaccounted[3,])){
      h <- max(cum_list$get(i)$Vaccounted[3,])
      method <- cum_list$get(i)
      method_name <- cum_list$keys[i]}
}

print(paste("Best Method: ", method_name, ",", "Cumsum: ", round(h, 3)))

fa.df.temp <- data.frame(method$scores)

write.csv(fa.df.temp, paste(ROOT_FOLDER, "/Pipeline/Output/factor_analysis.csv", sep = "") , row.names=F)

print(paste("Dataframe; with compontes is saved at: ", ROOT_FOLDER, "/Pipeline/Output/factor_analysis.csv", sep = ""))
