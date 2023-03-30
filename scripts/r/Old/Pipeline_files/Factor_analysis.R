#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0 ) {
  args[1] <- 2 }

library(psych)
library(Dict)
library(paran)

source(paste("/", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R", sep = ""))
pca.df.temporal <- read.csv(paste(ROOT_FOLDER,"/Output/ouput_pca.csv", sep = ""), row.names = 1)
df.gen <- read.csv(paste(ROOT_FOLDER, "/Input/General_information_13-07-2022_FULL.csv", sep = ""), row.names = 1)
change_NBB_Id <- function(df) {
  row.names(df) <- gsub(" ", "../..", row.names(df))
  row.names(df) <- gsub("-", "../..", row.names(df))
  return (df)
}

# df.gen[df.gen$Age == 0,]
# df.gen <- change_NBB_Id(df.gen)
#
# df.gen <- data.frame(row.names  = row.names(df.gen), Age = df.gen$Age)
# pca.df.temporal <- change_NBB_Id(pca.df.temporal)
# pca.df.temporal <- merge( pca.df.temporal, df.gen, by = 0, row.names = 0)

row.names(pca.df.temporal) <- pca.df.temporal$Row.names
pca.df.temporal$Row.names <- NULL

pa  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "pa",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))

ml  <- (fa.sapa(pca.df.temporal, nfactors = as.integer(args[1]) ,rotate= "varimax", fm = "ml",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))


wls  <- (fa(pca.df.temporal, nfactors = as.integer(args[1])  ,rotate= "varimax", fm = "wls",
                                  residuals = FALSE, SMC=FALSE,  n.iter=1))
# paran(pca.df.temporal, cfa = TRUE)

# fa.parallel(pa$scores, fa = "fa")


cum_list <- Dict$new(
                     "Principal Axisis"  = pa,
                     "Maximum likelihood"  = ml,
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
fa.loadings <- method$loadings
write.csv(fa.loadings, paste(ROOT_FOLDER, "/Output/fa_loadings.csv", sep = "") , row.names=F)
print(paste("Dataframe; with fa loadings is saved at: ", ROOT_FOLDER, "/Output/fa_loadings.csv", sep = ""))
