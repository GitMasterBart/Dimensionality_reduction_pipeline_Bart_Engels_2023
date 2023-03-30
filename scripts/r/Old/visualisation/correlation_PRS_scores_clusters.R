#!/usr/bin/env Rscript
library(plyr)
library(dplyr)
library(ggplot2)

source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R", sep = ""))

df.clusts <- read.csv(paste(ROOT_FOLDER, "/Output/cluster_information.csv", sep = ""), row.names = 1)
df.gen <- read.csv(paste(ROOT_FOLDER, "/Input/General_information_13-07-2022_FULL.csv", sep = ""), row.names = 1)
df.prs <- read.csv(paste(ROOT_FOLDER, "/Input/AD_scores.csv", sep = ""), row.names = 1, sep="\t")
df.prs <- df.prs %>% select_if(~ !any(is.na(.)))
df.prs$ID2 <- NULL
row.names(df.prs) <- paste("NBB", row.names(df.prs), sep = " ")
# check how manny clusters there are
unique(df.clusts$cluster)


l_AD <- c()
l_PD <- c()
l_MS <- c()
l_CON <- c()
for (i in 1:as.integer(length(unique(df.clusts$cluster)))){
  c <- tail(names(sort(table(df.clusts$main_diagnosis[df.clusts$cluster == i]))), 1)
  if (c == "AD")  l_AD <- append(l_AD, i)
  if ( c == "MS")  l_MS <- append(l_MS, paste("clust", i , sep = ""))
  if (c == "CON")  l_CON <- append(l_CON, paste("clust", i , sep = ""))
  if (c %in% c("PDD", "PD", "PSP"))  l_PD <- append(l_PD, i)
}
print(l_AD)
print(l_PD)
print(l_MS)
print(l_CON)
df.prs$z_scores <- (df.prs$Profile_1-mean(df.prs$Profile_1))/sd(df.prs$Profile_1)
l_AD <- c(1,2,3,4,5,6)
# filter out AD clusters
df.clusts.AD <- df.clusts[df.clusts$cluster %in% l_AD,]
df.clust.other <- df.clusts[df.clusts$cluster %in% c(1,6),]
# check dimensions
dim(df.clusts.AD[df.clusts.AD$main_diagnosis == "AD",])
dim(df.clusts.AD)
dim(df.clust.other[df.clust.other$main_diagnosis == "AD",])
# set type character cluster
df.clusts.AD$cluster <- as.character(df.clusts.AD$cluster)
# merge clusters 2,3,4,5 with prs scores
df.clusts.prs.AD <- merge(df.clusts.AD, df.prs, by = 0 )
row.names(df.clusts.prs.AD) <- df.clusts.prs.AD$Row.names
df.clusts.prs.AD$Row.names <- NULL
# plot prs scores clust 2,3,4,5

# calculate mu to show overlapping structures
mu <- ddply(df.clusts.prs.AD, "cluster", summarise, grp.mean=mean(z_scores))

df.clusts.prs.AD.gen <- merge(df.clusts.prs.AD, df.gen, by = 0)

ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "density_plot_prs_AD_F",".png", sep = ""),
       df.clusts.prs.AD.gen[df.clusts.prs.AD.gen$Gender == "F",] %>% ggplot(aes(x = z_scores, fill = cluster)) +
  geom_density(alpha=0.4) +
  theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = .5,
          margin = margin(10, 0, 30, 0)
        ),
        plot.caption = element_text(
          size = 9,
          color = "grey40",
          hjust = .5,
          margin = margin(20, 0, 5, 0)
        ),
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey88", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 15, color = "grey40"),
        legend.box.margin = margin(t = 30),
        legend.background = element_rect(
          color = "grey40",
          size = .3,
          fill = "grey95"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"),
        plot.margin = margin(rep(20, 4))
      )  +
      labs(
        title = paste("PRS density plot, F"),
        caption = "Made by • Bart Engels"
      ))

ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "density_plot_prs_AD_M",".png", sep = ""),
       df.clusts.prs.AD.gen[df.clusts.prs.AD.gen$Gender == "M",] %>% ggplot(aes(x = z_scores, fill = cluster)) +
  geom_density(alpha=0.4) +
  theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = .5,
          margin = margin(10, 0, 30, 0)
        ),
        plot.caption = element_text(
          size = 9,
          color = "grey40",
          hjust = .5,
          margin = margin(20, 0, 5, 0)
        ),
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey88", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 15, color = "grey40"),
        legend.box.margin = margin(t = 30),
        legend.background = element_rect(
          color = "grey40",
          size = .3,
          fill = "grey95"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"),
        plot.margin = margin(rep(20, 4))
      ) +
      labs(
        title = paste("PRS density plot, M"),
        caption = "Made by • Bart Engels"
      ))

ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "density_plot_prs_AD",".png", sep = ""),
       df.clusts.prs.AD.gen %>% ggplot(aes(x = z_scores, fill = cluster)) +
  geom_density(alpha=0.4) +
  theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = .5,
          margin = margin(10, 0, 30, 0)
        ),
        plot.caption = element_text(
          size = 9,
          color = "grey40",
          hjust = .5,
          margin = margin(20, 0, 5, 0)
        ),
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey88", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 15, color = "grey40"),
        legend.box.margin = margin(t = 30),
        legend.background = element_rect(
          color = "grey40",
          size = .3,
          fill = "grey95"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"),
        plot.margin = margin(rep(20, 4))
      ) +
      labs(
        title = paste("PRS density plot, ALL"),
        caption = "Made by • Bart Engels"
      ))

# merge gender to each cluster


