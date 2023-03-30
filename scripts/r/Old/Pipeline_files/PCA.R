#!/usr/bin/env Rscript
# Load libraries
library(dplyr)
library(ggplot2)
library(ggsignif)
library(tidyverse)
library(PCAtools)
library(MASS)
library(paran)
library(factoextra)
library(mvoutlier)

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
df.gen <- read.csv(paste(ROOT_FOLDER, "/Input/General_information_13-07-2022_FULL.csv", sep = ""), row.names = 1)
change_NBB_Id <- function(df) {
  row.names(df) <- gsub(" ", ".", row.names(df))
  row.names(df) <- gsub("-", ".", row.names(df))
  return (df)
}

# df.gen[df.gen$Age == 0,]
df.gen <- change_NBB_Id(df.gen)
df.temp <- change_NBB_Id(df.temp)
df.gen <- data.frame(row.names  = row.names(df.gen), Age = df.gen$Age)
df.temp <- merge( df.temp, df.gen, by = 0, row.names = 0)

row.names(df.temp) <- df.temp$Row.names
df.temp$Row.names <- NULL

df.temp <- df.temp[df.temp$Age > 21,]

dim(df.temp)
m.df <- as.matrix(df.temp[1:(dim(df.temp)[2]-3)])
all(m.df %in% 0:5)

# Print status
print("File loaded...")


# Replace Main_diagnosis values with translated values
for ( i in diseases.trans.dict$keys) {
  df.temp$Main_diagnosis[df.temp$Main_diagnosis == i] <- diseases.trans.dict$get(i)
}

#Determine the subset of data to be used
# subset <- c('CON',"AD","PD", "PDD", "DLB", "VD", "FTD",
#                                                  "PSP", "ATAXIA", "MS", "MSA", "MD", "BP", "SCHIZ")
# "OCD, PSYCH" , "ADHD, PSYCH"
# subset <- c("SCHIZ", "PSYCH", "DEPRI, PSYCH", "ASD, PSYCH", "PTSD, PSYCH" ,"NARCO, PSYCH", "PSYCH, DEPMA" ,"OCD, PSYCH" , "ADHD, PSYCH" )
subset <- c("AD", "CON")
if (length(args) == 3) {
  subset <-  char(args[3])
}


if (length(args) == 1) {
  subset <-  char(args[1])
}
#Print the subset used
print(paste("subseted on: ", subset))

df.temp <- subset(df.temp, Main_diagnosis %in% subset)
dim(df.temp)[2]
r_sum <- data.frame(rowSums(df.temp[1:(dim(df.temp)[2]-3)]))

# only neede when splitting on Gender
# data_frame.temp <- data_frame.temp[data_frame.temp$Gender == "M",]

# Write the subsetted dataframe to file
# row_names_df_to_remove <- c("NBB 2017-076", "NBB 2008-061", "NBB 2019-122", "NBB 2016-061")
#
# data_frame.temp <- data_frame.temp[!(row.names(data_frame.temp) %in% row_names_df_to_remove),]

write.csv(df.temp, paste(ROOT_FOLDER, "/Output/subseted_df.csv", sep = "") , row.names=T)

# Print status
print(paste("Dataframe subset is saved at: ", ROOT_FOLDER, "/Output/subseted_df.csv", sep = ""))

print("File stripped....")


# p <- parallelPCA(cov(log1p(df.temp[1:(dim(df.temp)[2]-2)])))
# # # Perform PCA
# print(p)
# #
# paran(df.temp[1:(dim(df.temp)[2]-2)],cfa = TRUE , graph = TRUE, color = TRUE,
#       col = c("black", "red", "blue"))

pca.temp.t <- prcomp(df.temp[1:(dim(df.temp)[2]-3)], scale. = F)
elbow <- findElbowPoint(pca.temp.t$sdev)
print(elbow)
dim(df.temp)
print(which(cumsum(pca.temp.t$sdev) > 80)[1])

identify(qqnorm(pca.temp.t$x[,2],pch = 20, col = c(rep("red", 33), rep("blue", 99))))

#
f <- fviz_pca_ind(pca.temp.t,
         col.ind = "contrib", # Color by contribution
         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") #assign gradient
         )
print(f)
# #
print(fviz_eig(pca.temp.t,
         addlabels=TRUE))



ggplot_pcaplot_sub(pca.temp.t, df.temp, 1,2,list_with_colored_diagnoses = subset ,name = " ", scale = .7 )
ggplot_pcaplot_sub(pca.temp.t, df.temp, 1,4,list_with_colored_diagnoses = subset ,name = " ", scale = .7 )
ggplot_pcaplot_sub(pca.temp.t, df.temp, 15,16,list_with_colored_diagnoses = subset ,name = " ", scale = .7 )

# pca.temp.t <- pca(data_frame.temp[1:(dim(data_frame.temp)[2]-1)])
# var_explained <- pca.temp.t$sdev^2 / sum(pca.temp.t$sdev^2)
#
ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "screeplot", ".png", sep = ""), fviz_eig(pca.temp.t,
         addlabels=TRUE))
# Print status
print("PCA performed......")
# Determine the components to be used
if (length(args) <= 1) {
  componet_list <- list.pc.symptom.corr(pca.temp.t, df.temp)
  print(paste("List with components with differatiating abbility: ", list(componet_list)))
}
if (length(args) >= 2) componet_list <- c(args[1]:args[2])

print(paste("Saved componeets: ", list(componet_list)))

pca.df.temporal<- data.frame(row.names = row.names(df.temp), pca.temp.t$x[,componet_list])

write.csv(pca.df.temporal, paste(ROOT_FOLDER, "/Output/ouput_pca.csv", sep = "") , row.names=T)

print(paste("Dataframe; with compontes is saved at: ", ROOT_FOLDER, "/Output/ouput_pca.csv", sep = "" ))
