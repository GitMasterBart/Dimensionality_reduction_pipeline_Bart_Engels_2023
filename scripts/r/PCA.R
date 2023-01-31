#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(ggsignif)


args <- commandArgs(trailingOnly=TRUE)
source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Pipeline/scripts/r/file_names.R", sep = ""))
source(paste(ROOT_FOLDER, "/Pipeline/scripts/r/pca_differantiablity_detection.R", sep = ""))
source(paste(ROOT_FOLDER,"/Pipeline/scripts/r/table_files/table1.R", sep = ""))
source(paste(ROOT_FOLDER, "/Pipeline/scripts/r/graphs/plots_factor_analysis.R", sep = ""))

df.temp <- read.csv(paste(ROOT_FOLDER, TEMPORAL_FILE, sep = ""), row.names = 1)

print("File loaded...")

for ( i in diseases.trans.dict$keys) {
  df.temp$Main_diagnosis[df.temp$Main_diagnosis == i] <- diseases.trans.dict$get(i)
}



if (length(args) == 2) {
  subset <- c('CON',"AD","PD", "PDD", "DLB", "VD", "FTD",
                                                 "PSP", "ATAXIA", "MS", "MSA", "MD", "BP", "SCHIZ")
}

if (length(args) == 3) {
  subset <-  char(args[3])
}

print(paste("subseted on: ", subset))

df.temp <- subset(df.temp, Main_diagnosis %in% subset)

write.csv(df.temp, paste(ROOT_FOLDER, "/Pipeline/Output/subseted_df.csv", sep = "") , row.names=F)

print(paste("Dataframe subset is saved at: ", ROOT_FOLDER, "/Pipeline/Output/subseted_df.csv" ))

print("File stripped....")

pca.temp.t <- prcomp(df.temp[1:(dim(df.temp)[2]-1)], scale. = F)

print("PCA performed......")




if (length(args)==0) {
  componet_list <- list.pc.symptoom.corr(pca.temp.t, df.temp)
  print(paste("List with components with differatiating abbility: ", list(componet_list)))
}

if (length(args) == 2) componet_list <- c(args[1]:args[2])
if (length(args) == 3) componet_list <- c(args[1]:args[2])

print(paste("Saved componeets: ", list(componet_list)))

pca.df.temporal<- data.frame(row.names = row.names(df.temp), pca.temp.t$x[,componet_list])

write.csv(pca.df.temporal, paste(ROOT_FOLDER, "/Pipeline/Output/ouput_pca.csv", sep = "") , row.names=F)

print(paste("Dataframe; with compontes is saved at: ", ROOT_FOLDER, "/Pipeline/Output/ouput_pca.csv" ))
