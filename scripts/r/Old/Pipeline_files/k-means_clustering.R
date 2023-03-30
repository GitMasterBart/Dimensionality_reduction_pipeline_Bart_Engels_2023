# Apply writeLines function
library(NbClust)
library(ggplot2)
library(ggstream)
library(colorspace)
library(tidyverse)

args <- commandArgs(trailingOnly=TRUE)


#test if there is at least one argument: if not ch is defeault
if (length(args)==0) {
  args[1] <- "ch"
}

print(args[1])

source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R",sep = ""))
source(paste(ROOT_FOLDER, "/scripts/r/graphs/temporal_data_visualiser.R", sep = ""))
source(paste(ROOT_FOLDER, "/scripts/r/graphs/plots_factor_analysis.R", sep = ""))
source(paste(ROOT_FOLDER, "/scripts/r/graphs/stackbarplot.R", sep = ""))
writeLines(args[1], paste(ROOT_FOLDER ,"/Output/img/clusts/", "cluster_method.txt", sep = ""))


fa.df.temp <- read.csv(paste(ROOT_FOLDER, "/Output/factor_analysis.csv", sep = ""))
# row.names(data_frame.thumb) <- data_frame.thumb$X
df.temp <-  read.csv(paste(ROOT_FOLDER, "/Output/subseted_df.csv", sep = ""), row.names = 1)
#ch
res.2 <- NbClust(fa.df.temp, distance = "euclidean", min.nc = 2, max.nc = 40, method = "ward.D2", index = args[1])

years_end <- 102

#as.integer(res.2$Best.nc[1])
km.temp.4 <- kmeans(fa.df.temp, as.integer(res.2$Best.nc[1]) , nstart=25)
df.temp.fa.clusters.4 <-
  data.frame(row.names = row.names(df.temp), fa.df.temp,
             cluster = km.temp.4$cluster,
             main_diagnosis = df.temp$Main_diagnosis)


write.csv(df.temp.fa.clusters.4, paste(ROOT_FOLDER, "/Output/cluster_information.csv", sep = "") , row.names=T)
# data_frame.temp.fa.clusters.4 <- read.csv(paste(ROOT_FOLDER, "/Output/cluster_information.csv", sep = ""))

ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "clusts",".png", sep = ""), stack_bar_plot(df.temp.fa.clusters.4) , limitsize = FALSE)

# for (i in length(unique(data_frame.temp.fa.clusters.4$cluster))) {
#   ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "clusts", "symp_coherance_", i, ".png", sep = ""),
#   ggplot_symptomp_coherence(data_frame.thumb[2:80], data_frame.temp.fa.clusters.4[data_frame.temp.fa.clusters.4$cluster == i,] ))
# }
# data_frame.temp.fa.clusters.4 <- read.csv(paste(ROOT_FOLDER, "/Output/cluster_information.csv", sep = ""))
ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "bartplot_clusters",".png", sep = ""), ggplot(data.frame(table(df.temp.fa.clusters.4$cluster)), aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1"))



df.info <- list()
for (i in c(1:as.integer(res.2$Best.nc[1]))){
  clust.temp <- df.temp.fa.clusters.4[df.temp.fa.clusters.4$cluster == i,]
  clust_col_sum <- (colSums(df.temp[row.names(df.temp) %in% row.names(clust.temp),][1:(dim(df.temp)[2]-3)]) / 5) / (dim(clust.temp)[1]) * 100
  df.info[[paste0("clust", i)]] <- clust_col_sum
}
df.info <- do.call(cbind, df.info)

# write.csv(totol.data_frame,"/Users/bengels/Desktop/stage_umcg2022/scripts/Dimensionality_reduction_pipeline_Bart_Engels_2023/Output/ouput_pca_fa_kmeans_analysis.csv" , row.names=F)

extract_year_symptom <- function(df) {
  df$names  <- row.names(df)
  df$Year <- gsub("[a-z.]+", "", row.names(df))
  df$Year <- gsub("[A-Z_]+", "", df$Year)
  df$Year <- as.double(df$Year)
  df$symptom <- gsub("[X]", "", row.names(df) )
  df$symptom <- gsub("[0-9]+.", "", df$symptom )
  return(df)
}

df_char_vis <- extract_year_symptom(data.frame(df.info))

l_AD <- c()
l_PD <- c()
l_MS <- c()
l_CON <- c()
for (i in 1:as.integer(length(colnames(df.info)))){
  c <- tail(names(sort(table(df.temp.fa.clusters.4$main_diagnosis[df.temp.fa.clusters.4$cluster == i]))), 1)

  if (c == "AD")  l_AD <- append(l_AD, paste("clust", i , sep = ""))
  if ( c == "MS")  l_MS <- append(l_MS, paste("clust", i , sep = ""))
  if (c == "CON")  l_CON <- append(l_CON, paste("clust", i , sep = ""))
  if (c %in% c("PDD", "PD", "PSP"))  l_PD <- append(l_PD, paste("clust", i , sep = ""))
}

if (length(l_AD) != 0 ) linegraph_temporal_data("AD",l_AD,years_end, "rev")
print("set done...")
if (length(l_PD) != 0 ) linegraph_temporal_data("PD",l_PD,years_end, "rev")
print("set done...")
if (length(l_MS) != 0 ) linegraph_temporal_data("MS",l_MS,years_end, "rev")
print("set done...")
if (length(l_CON) != 0 ) linegraph_temporal_data("CON",l_CON,years_end, "rev")
print("set done...")
linegraph_temporal_data("ALL",colnames(df.info),years_end, "rev")
print("Finished....")
