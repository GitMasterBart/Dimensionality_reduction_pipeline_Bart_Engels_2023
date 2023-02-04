#!/usr/bin/env Rscript
# Load libraries
library(dplyr)
library(ggplot2)
library(ggsignif)
library(tidyverse)
library(PCAtools)
library(MASS)
library(paran)


#Get command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Load required R scripts
source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R", sep = ""))
source(paste(ROOT_FOLDER, "/scripts/r/pca_differantiablity_detection.R", sep = ""))
source(paste(ROOT_FOLDER,"/scripts/r/table_files/table1.R", sep = ""))
source(paste(ROOT_FOLDER, "/scripts/r/graphs/plots_factor_analysis.R", sep = ""))

# for now
source("/Users/bengels/Desktop/stage_umcg2022/scripts/r-scripts/Graph_files/ggplot_functions.R")



# Read CSV file
df.temp <- read.csv(paste(ROOT_FOLDER, TEMPORAL_FILE, sep = ""), row.names = 1)
m.df <- as.matrix(df.temp[1:dim(df.temp)[1]-1])
all(m.df %in% 0:1)

# Print status
print("File loaded...")


# Replace Main_diagnosis values with translated values
for ( i in diseases.trans.dict$keys) {
  df.temp$Main_diagnosis[df.temp$Main_diagnosis == i] <- diseases.trans.dict$get(i)
}


#Determine the subset of data to be used
subset <- c('CON',"AD","PD", "PDD", "DLB", "VD", "FTD",
                                                 "PSP", "ATAXIA", "MS", "MSA", "MD", "BP", "SCHIZ")
#subset <- c("MD", "SCHIZ", "PSYCH", "DEPRI, PSYCH", "DEV", "ASD, PSYCH", "ADHD, PSYCH", "PTSD, PSYCH" , "OCD, PSYCH","NARCO, PSYCH", "PSYCH, DEPMA" )

if (length(args) == 3) {
  subset <-  char(args[3])
}

#Print the subset used
print(paste("subseted on: ", subset))

df.temp <- subset(df.temp, Main_diagnosis %in% subset)

# Write the subsetted dataframe to file
write.csv(df.temp, paste(ROOT_FOLDER, "/Output/subseted_df.csv", sep = "") , row.names=T)

# Print status
print(paste("Dataframe subset is saved at: ", ROOT_FOLDER, "/Output/subseted_df.csv", sep = ""))

print("File stripped....")


# p <- parallelPCA(cov(df.temp[1:(dim(df.temp)[2]-1)]))
# Perform PCA

pca.temp.t <- prcomp(df.temp[1:(dim(df.temp)[2]-1)], scale. = F)

ggplot_pcaplot_sub(pca.temp.t, df.temp, 1,2,list_with_colored_diagnoses = subset ,name = " ", scale = .7 )
ggplot_pcaplot_sub(pca.temp.t, df.temp, 3,4,list_with_colored_diagnoses = subset ,name = " ", scale = .7 )


# pca.temp.t <- pca(df.temp[1:(dim(df.temp)[2]-1)])

var_explained_df <- data.frame(PC= paste0("PC",1:5),
                               var_explained=(pca.temp.t$sdev)^2/sum((pca.temp.t$sdev)^2))


ggsave(paste(ROOT_FOLDER ,"/Output/img/screeplot", ".png"),
       ggplot(var_explained_df, aes( x=PC,y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data"))



# Print status
print("PCA performed......")
# Determine the components to be used
if (length(args) == 0) {
  componet_list <- list.pc.symptom.corr(pca.temp.t, df.temp)
  print(paste("List with components with differatiating abbility: ", list(componet_list)))
}
if (length(args) >= 2) componet_list <- c(args[1]:args[2])

print(paste("Saved componeets: ", list(componet_list)))

pca.df.temporal<- data.frame(row.names = row.names(df.temp), pca.temp.t$x[,componet_list])

write.csv(pca.df.temporal, paste(ROOT_FOLDER, "/Output/ouput_pca.csv", sep = "") , row.names=F)

print(paste("Dataframe; with compontes is saved at: ", ROOT_FOLDER, "/Output/ouput_pca.csv", sep = "" ))
