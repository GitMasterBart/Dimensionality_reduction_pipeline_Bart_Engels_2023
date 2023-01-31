#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

#test if there is at least one argument: if not, return an error
# if (length(args)==0) {
#   stop("At least one argument must be supplied (input file).n", call.=FALSE)
# } else if (length(args)==1) {
#   # default Output file
#   args[2] <- c(args:5)}
library(NbClust)


source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Pipeline/scripts/r/file_names.R",sep = ""))

fa.df.temp <- read.csv(paste(ROOT_FOLDER, "/Pipeline/Output/factor_analysis.csv", sep = ""))

df.temp <-  read.csv(paste(ROOT_FOLDER, "/Pipeline/Output/subseted_df.csv", sep = ""))

res.2 <- NbClust(fa.df.temp, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2", index = "ch")

print(res.2$Best.nc[1])
#manhattan euclidean
# res <- NbClust(fa.df.temp, distance = "binairy", min.nc = 2, max.nc = 15, method = "ward.D2", index = "ch")
#
# print(res$Best.nc[1])


km.temp.4 <- kmeans(fa.df.temp, as.integer(res.2$Best.nc[1]), nstart=25)
df.temp.fa.clusters.4 <-
  data.frame(row.names = row.names(df.temp), fa.df.temp,
             cluster = km.temp.4$cluster,
             main_diagnosis = df.temp$Main_diagnosis)

ggplot(df.temp.fa.clusters.4, aes(fill=main_diagnosis, y=abs(cluster), x=cluster)) +
           geom_bar(position="fill", stat="identity") +
           ggtitle("") +
           scale_fill_manual(values = c("CON" = "#6E3562",
                                        "AD" ="#E66912",
                                        "FTD" = "#016367",
                                        "MS" = "#8CA252",
                                        "PD"= "#9E3A14",
                                        "MND"= "#D43649",
                                        "BP"= "#2a9d8f",
                                        "MD"= "#9e2a2b",
                                        "PDD"= "#C68B5E",
                                        "DLB" = "#1d2d44",
                                        "PSP" = "#d1495b",
                                        "ATAXIA" = "#8390fa",
                                        "MSA" = "#FBCE3A",
                                        "MD" = "#00cecb",
                                        "SCHIZ" = "#008000",
                                        "VD" = "red"
           ), name = "Diagnosis:") +
  theme_classic() +
    xlab("")


# gap_stat <- clusGap(fa.df.temp, FUN = kmeans, nstart = 25,
#                      K.max = 10, B = 10)

# (fa.df.temp)
# fviz_nbclust(fa.df.temp, kmeans, method = "silhouette")

#  fviz_gap_stat(gap_stat)

#  fviz_nbclust(fa.df.temp, kmeans, method = "wss")



df.info <- list()
for (i in c(1:as.integer(res.2$Best.nc[1]))){
  clust.temp <- df.temp.fa.clusters.4[df.temp.fa.clusters.4$cluster == i,]
  clust_col_sum <- (colSums(df.temp[row.names(df.temp) %in% row.names(clust.temp),][1:(dim(df.temp)[2]-3)]) / 1) / (dim(clust.temp)[1]) * 100
  df.info[[paste0("clust", i)]] <- clust_col_sum
}
df.info <- do.call(cbind, df.info)

# write.csv(totol.df,"/Users/bengels/Desktop/stage_umcg2022/scripts/Dimensionality_reduction_pipeline_Bart_Engels_2023/Output/ouput_pca_fa_kmeans_analysis.csv" , row.names=F)

extract_year_symptom <- function(df) {
  df$names  <- row.names(df)
  df$Year <- gsub("[a-z.]+", "", row.names(df))
  df$Year <- gsub("[A-Z_]+", "", df$Year)
  df$Year <- as.double(df$Year)
  df$symptom <- gsub("[X]", "", row.names(df) )
  df$symptom <- gsub("[0-9]+.", "", df$symptom )
  return(df)
}

df_char_vis$symptom


levels = c("Dementia")
end.year = 40
clusts <- colnames(df.info)
df_char_vis <- extract_year_symptom(data.frame(df.info))


  create_df_best_chars <- function(symptom_list){
    df_best_chars <- tibble(
      rank = 1:length(symptom_list),
      char_popular = symptom_list)
    return(df_best_chars)
  }
  pal <- c(
    "#8CA252", lighten("#8CA252", .25, space = "HLS"),
    "#809c13", lighten("#809c13", .2, space = "HLS"),
    "#607c3c", lighten("#607c3c", .2, space = "HLS")

  )

  pal <- c(
    "#FFB400", lighten("#8CA252", .25, space = "HLS"),
    "#C20008", lighten("#C20008", .2, space = "HLS"),
    "#13AFEF", lighten("#595A52", .25, space = "HLS"),
    "#8E038E", lighten("#8E038E", .2, space = "HLS"),
    "#595A52", lighten("#607c3c", .15, space = "HLS"),
    "#8CA252", lighten("#8CA252", .25, space = "HLS"),
    "#809c13", lighten("#809c13", .2, space = "HLS"),
    "#607c3c", lighten("#607c3c", .2, space = "HLS")

  )

  # df_char_vis <- df_char_vis %>%
  #   relocate(clust2, .before = clust4 )


  df <- df_char_vis[df_char_vis$symptom %in% levels,] %>%
    filter(Year < end.year)



  df_smooth <- df %>%
    group_by(symptom)  %>%
    slice(1:4) %>%
    mutate(
      Year = c(
        min(df$Year) - 2,
        min(df$Year) - .5,
        max(df$Year) + .5,
        max(df$Year) + 2
      ),
      clust1 = c(0, .001, .001, 0),
      clust2 = c(0, .001, .001, 0),
      clust3 = c(0, .001, .001, 0),
      clust4 = c(0, .001, .001, 0),
      clust5 = c(0, .001, .001, 0)
    )



  df_best_stream_fct <- df %>%
    bind_rows(df) %>%
    mutate(

      char_costume =  factor(symptom, levels = levels) ,


      #char_costume = fct_reorder(char_costume, rank)
    ) %>%
    pivot_longer(
      cols = clusts,
      names_to = "parameter",
      values_to = "value"
    ) #%>%
  #mutate(parameter = factor(parameter, levels = levels))






  g <-df_best_stream_fct %>%
    ggplot(aes(Year, value,
               color = parameter,
                fill = parameter)) +
    geom_line() +
    geom_line(
      geom = "polygon",
      bw = .65,
      size = 0
    ) +
    scale_color_manual(
      expand = c(0, 0),
      values = pal
    ) +
    scale_fill_manual(
      values = pal
  )+
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
      legend.text = element_text(size = 9, color = "grey40"),
      legend.box.margin = margin(t = 30),
      legend.background = element_rect(
        color = "grey40",
        size = .3,
        fill = "grey95"
      ),
      legend.key.height = unit(.25, "lines"),
      legend.key.width = unit(2.5, "lines"),
      plot.margin = margin(rep(20, 4))
    )

  g <- g +
    geom_vline(
      data = tibble(x = c(end.year, seq(end.year, 0, by = -5), 0)),
      aes(xintercept = x),
      inherit.aes = FALSE,
      color = "grey88",
      size = .5,
      linetype = "dotted"
    ) +
    annotate(
      "rect",
      xmin = -Inf, xmax = 0,
      ymin = -Inf, ymax = Inf,
      fill = "grey88"
    ) +
    annotate(
      "rect",
      xmin = end.year, xmax = Inf,
      ymin = -Inf, ymax = Inf,
      fill = "grey88"
    ) +
    scale_x_continuous(
      limits = c(0, NA),
      breaks = c(end.year, seq(end.year, 0, by = -5), 0),
      labels = glue::glue("Year\n{c(-0, seq(0, -{end.year}, by = -5), -{end.year})}"),
      position = "top"
    ) +
    # Customize labels of the horizontal axis
    #scale_y_continuous(expand = c(.03, .03)) +
    # This clip="off" is very important. It allows to have annotations anywhere
    # in the plot, no matter they are not within the extent of
    # the corresponding panel.
    coord_cartesian(clip = "off") +
    scale_x_reverse()


  g <- g +
    labs(
      caption = "Made by • Bart Engels"
    )
g