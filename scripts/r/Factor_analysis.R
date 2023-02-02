#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0 ) {
  args[1] <- 3 }

library(psych)
library(Dict)
library(paran)

source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R", sep = ""))
pca.df.temporal <- read.csv(paste(ROOT_FOLDER,"/Output/ouput_pca.csv", sep = ""))

p <- paran(pca.df.temporal, centile = 0, quietly = FALSE,
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE,
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE,
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)



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
                      "minimum rank "= minrank,
                      "Weighted least squared" = wls)

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

write.csv(fa.df.temp, paste(ROOT_FOLDER, "/Output/factor_analysis.csv", sep = "") , row.names=F)

print(paste("Dataframe; with compontes is saved at: ", ROOT_FOLDER, "/Output/factor_analysis.csv", sep = ""))
