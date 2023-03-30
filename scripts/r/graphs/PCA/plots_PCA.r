
screePlot <- function(variance, intercept ) {
  # scree plot PCA returns the variance for range is 10 pcs
  return(qplot(x = c(1:10), y = variance[1:10]) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=variance[intercept], linetype="dashed", color = "blue") +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))+
  theme_classic())}

ggplot_pcaplot_sub_elip <- function(pca, dataf, pc_x, pc_y, list_with_colored_diagnoses, name, scale) {

  # returns a bi plot with elips,
  # variables:
  # PCA : PCA performed with pcromp
  # df, including Main_diagnosis, Age, Gender
  # pc_x: PC you want to visualize on the x-axisis
  # Pc_y: PC you want ot visualize on te y-axisis
  # name: String for the MAIN (Title)
  # scale: scale the Biplot so the signs/symptoms are readable
  library(ggplot2)

  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pca$x ,Main_diagnosis= dataf$Main_diagnosis, Gender = dataf$Gender)
  #print(gg.ready.pca.data.norm.log1p)

  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)

  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)


  f <- which.max(pca$rotation[,pc_x])
  sec <- which.min(pca$rotation[,pc_x])
  three <-which.max(pca$rotation[,pc_y])
  four <- which.min(pca$rotation[,pc_y])

  # Create matrix of x coordinates (PC1) and multiply by 10
  l.x <- cbind(pca$rotation[,pc_x][c(f, sec, three , four)]) * (10 * scale)
  # y coordinates (PC2)
  l.y <- cbind(pca$rotation[,pc_y][c(f, sec, three , four)]) * (10 * scale)

  PCAloadings <- data.frame( Variables = row.names(l.x), x = l.x, y = l.y)

  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = Main_diagnosis )) +
    geom_point(aes(shape = Gender))+
    stat_ellipse() +
    stat_ellipse(type = "t", linetype = 2) +
    # stat_ellipse(type = "t") +
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) +
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_manual(values = c("CON" = "#6E3562",
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
                                        "VD" = "red",
                                        "PTSD, PSYCH" = "blue",
                                        "ASD, PSYCH" = "#E5FFCC",
                                        "DEPRI, PSYCH" = "#FF4500",
                                        "DEV" = "#00FF7F",
                                        "ADHD, PSYCH" = "#191970",
                                        "PSYCH" = "#FF00FF",
                                        "OCD, PSYCH" = "#6B8E23",
                                        "NARCO, PSYCH" = "#D2691E",
                                        "PSYCH, DEPMA" =  "#DEB887"
                                  ), limits = list_with_colored_diagnoses) +
    scale_shape_manual(values=c( "F" = 16,
                                 "M" = 17)) +
    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (l.x), yend = (l.y)), arrow = arrow(length = unit(1/2, "picas")), color = "red") +
    annotate("text", x = (l.x), y = (l.y),
             label = PCAloadings$Variables)
  return(ggplot_dynamic)}



ggplot_pcaplot_sub <- function(pca, dataf, pc_x, pc_y, list_with_colored_diagnoses, name, scale) {
  # returns a bi-plot
  # variables:
  # PCA : PCA performed with pcromp
  # df, including Main_diagnosis, Age, Gender
  # pc_x: PC you want to visualize on the x-axisis
  # Pc_y: PC you want ot visualize on te y-axisis
  # name: String for the MAIN (Title)
  # scale: scale the Biplot so the signs/symptoms are readable
  library(ggplot2)


  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pca$x ,Main_diagnosis= dataf$Main_diagnosis)
  #print(gg.ready.pca.data.norm.log1p)

  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)

  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)


  f <- which.max(pca$rotation[,pc_x])
  sec <- which.min(pca$rotation[,pc_x])
  three <-which.max(pca$rotation[,pc_y])
  four <- which.min(pca$rotation[,pc_y])

  # Create matrix of x coordinates (PC1) and multiply by 10
  l.x <- cbind(pca$rotation[,pc_x][c(f, sec, three , four)]) * (10 * scale)
  # y coordinates (PC2)
  l.y <- cbind(pca$rotation[,pc_y][c(f, sec, three , four)]) * (10 * scale)

  PCAloadings <- data.frame( Variables = row.names(l.x), x = l.x, y = l.y)

  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = Main_diagnosis )) +
    geom_point(size = 3)+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) +
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="") ) +
    theme_bw() +
    guides(fill=guide_legend(title="New Legend Title"))+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=25),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25))+
    scale_color_manual(values = c("CON" = "#6E3562",
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
                                        "VD" = "red",
                                        "PTSD, PSYCH" = "blue",
                                        "ASD, PSYCH" = "#E5FFCC",
                                        "DEPRI, PSYCH" = "#FF4500",
                                        "DEV" = "#00FF7F",
                                        "ADHD, PSYCH" = "#191970",
                                        "PSYCH" = "#FF00FF",
                                        "OCD, PSYCH" = "#6B8E23",
                                        "NARCO, PSYCH" = "#D2691E",
                                        "PSYCH, DEPMA" =  "#DEB887"
                                  ), limits = list_with_colored_diagnoses, name = "Diagnosis") +

    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (l.x), yend = (l.y)), arrow = arrow(length = unit(1, "picas")), color = "red") +
    annotate("text", x = (l.x), y = (l.y),
             label = PCAloadings$Variables, size = 5, face="bold", color = "blue")
  return(ggplot_dynamic)}


own_heatmap <- function(pca,pc.x, pc.y, f.1,f.2  ){
  # create a heatmap
  # input:
  # pc.x and pc.y, returns object like [,1:4] so the first 4
  # pc are used in this case.
  # f.1 and f.2 creat togehter a list with a range of
  # which features are showcased.
  library(ComplexHeatmap)
  library(RColorBrewer)
  .boxplot1 = anno_boxplot(pca$rotation[,pc.x:pc.y][f.1:f.2,], which = "row")
  ha_mix_right1 = HeatmapAnnotation(bxplt = .boxplot1,
                              which = "row", width = unit(4, "cm"))
  heatmap <- Heatmap(pca$rotation[,pc.x:pc.y][f.1:f.2,],
        name = "scale",
        column_title = "Variables", row_title = "Samples",
        col = colorRampPalette(brewer.pal(8, "Blues"))(25),
       column_names_gp = gpar(fontsize = 8),
      left_annotation = ha_mix_right1,
       show_heatmap_legend = T)
  return (heatmap)

}