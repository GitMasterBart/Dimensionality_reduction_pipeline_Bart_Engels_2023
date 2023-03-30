ggplot_pcaplot_sub_v1 <- function(pca, dataf, pc_x, pc_y, list_with_colored_diagnoses, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4], Main_diagnosis= dataf$Main_diagnosis)
  #print(gg.ready.pca.data.norm.log1p)
  
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = Main_diagnosis )) + 
    geom_point()+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_manual(values = c("CON" = "purple",
                                  "AD" ="orange",
                                  "FTD" = "steelblue",
                                  "MS" = "red",
                                  "PD"= "green"), limits = list_with_colored_diagnoses)
  return(ggplot_dynamic)}

ggplot_pcaplot_sub <- function(pca, dataf, pc_x, pc_y, list_with_colored_diagnoses, name, scale) {
  
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
    # scale_color_viridis(discrete=TRUE, option="viridis") +

    # ggtitle(paste("PCA plot of", pcx ,"and", pcy, "of", name )) +
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


ggplot_pcaplot_sub_elip <- function(pca, dataf, pc_x, pc_y, list_with_colored_diagnoses, name, scale) {

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


ggplot_pcaplot_sex <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pca$x, Gender= dataf$Gender)
  
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = Gender )) + 
    geom_point()+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_manual(values = c("M" = "blue", 
                                  "F" = "pink"))
  return(ggplot_dynamic)}




ggplot_pcaplot_domein <- function(pca, dataf, pc_x, pc_y,list_with_colored_diagnoses, name, scale) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], Gender = dataf$Gender, Main_diagnosis= dataf$Main_diagnosis)

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
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_manual(values = c("CON" = "pink",
                                  "AD" ="#E7BA52",
                                  "FTD" = "#E7BA52",
                                  "VD" = "#E7BA52",
                                  "MS" = "#8CA252",
                                  "PD"= "#393B79",
                                  "MSA" = "#393B79",
                                  "MND"= "#393B79",
                                  "PDD" = "#393B79",
                                  "BP"= "#AD494A",
                                  "MD"= "#AD494A",
                                  "SCHIZ"= "#AD494A",
                                  "PSP"= "#393B79"
    ), limits = list_with_colored_diagnoses) +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17)) +
    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (l.x), yend = (l.y)), arrow = arrow(length = unit(1/2, "picas")), color = "red") +
    annotate("text", x = (l.x), y = (l.y),
             label = PCAloadings$Variables)
  
  return(ggplot_dynamic)}

#("#E7BA52", "#8CA252","#AD494A","#393B79" ,"#A55194")


ggplot_pcaplot_gradiant_char <- function(pca, dataf, pc_x, pc_y, name, limits) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], Char_count_ch= dataf$char_dos, Gender = dataf$Gender)
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = Char_count_ch , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red", limits=c(0,limits)) +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}


ggplot_pcaplot_gradiant_dosAge <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], dos_age= dataf$dos_age, Gender = dataf$Gender)
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = dos_age , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red") +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}

ggplot_pcaplot_gradiant_info_densi <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], inf.dens= dataf$inf.dens, Gender = dataf$Gender)
  
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = inf.dens , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red", limits = c(0, 0.025)) +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}


ggplot_pcaplot_gradiant_symptom_count <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], symptom_freq= dataf$sum, Gender = dataf$Gender)
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = symptom_freq , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red") +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}


ggplot_pcaplot_domein_ellipse <- function(pca, dataf, pc_x, pc_y,list_with_colored_diagnoses, name, scale) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf),pca$x, Gender = dataf$Gender, Main_diagnosis= dataf$Main_diagnosis)
  
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  f <- which.max(pca$rotation[,pc_x])
  sec <- which.min(pca$rotation[,pc_x])
  three <-which.max(pca$rotation[,pc_y])
  four <- which.min(pca$rotation[,pc_y])
  
  cog <- c("AD", "FTD", "VD")
  mot <- c("PD", "PDD", "MND","PSP", "ATAXIA", "MD" )
  spy <- c("BP", "SCHIZ","MDD")
  sen <- c("MS","MSA")
  con <- c("CON")
  gg.ready.pca.data.norm.log1p$cog <- ifelse(gg.ready.pca.data.norm.log1p$Main_diagnosis %in% cog, yes = "cognitive", no =  ifelse(gg.ready.pca.data.norm.log1p$Main_diagnosis %in% mot, yes = "motor", no =  ifelse(gg.ready.pca.data.norm.log1p$Main_diagnosis %in% spy, yes = "psychatric", no =  ifelse(gg.ready.pca.data.norm.log1p$Main_diagnosis %in% sen, yes = "sensory", no =  ifelse(gg.ready.pca.data.norm.log1p$Main_diagnosis %in% con, yes = "CON", no = "no")))))
  # Create matrix of x coordinates (PC1) and multiply by 10
  l.x <- cbind(pca$rotation[,pc_x][c(f, sec, three , four)]) * (10 * scale)
  # y coordinates (PC2)
  l.y <- cbind(pca$rotation[,pc_y][c(f, sec, three , four)]) * (10 * scale)
  PCAloadings <- data.frame( Variables = row.names(l.x), x = l.x, y = l.y)
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = cog )) + 
    geom_point(aes(shape = Gender))+
    stat_ellipse() + 
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_manual(values = c(
      "cognitive" = "#E7BA52",
      "CON" = "pink",
      
      "sensory" = "#8CA252",
      "motor"= "#393B79",
      "psychatric"= "#AD494A"
      
    ), limits = c("cognitive", "CON", "sensory", "motor", "psychatric") ) + 
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17)) +
    geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (l.x), yend = (l.y)), arrow = arrow(length = unit(1/2, "picas")), color = "red") +
    annotate("text", x = (l.x), y = (l.y),
             label = PCAloadings$Variables)
  
  return(ggplot_dynamic)}

heatmap.creater <- function(pca, df, i){
  # greates heatmaps
  df.v <- df.violin.plot(pca, df,  i)
  main_diagnosis <- factor(df.v$text, labels = unique(df.v$text))
  res <- pairwise.wilcox.test(df.v$value, main_diagnosis, p.adjust.method = "bonferroni", exact=FALSE)
  return(melt(res$p.value) %>%
           ggplot(aes(X1, X2, fill = value)) 
         + theme_bw() 
         + theme_classic()
         + geom_tile()+ 
           scale_fill_gradient2( midpoint = .5 ,  limit = c(0,1), 
                                 name="p-value\nMann-Whitney\nU") + 
           xlab(paste("PC" , i)) + 
           ylab(""))}

age_ggplot <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pc1 =pca$x[,1], pc2 = pca$x[,2],pc3 =pca$x[,3], pc4 =pca$x[,4],pc5 =pca$x[,5], pc6 =pca$x[,6],
                                             pc7 =pca$x[,7],pc8 =pca$x[,8],pc9 =pca$x[,9],pc10 =pca$x[,10], age= dataf$Age, Gender = dataf$Gender)
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = age , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red") +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}

age_ggplot <- function(pca, dataf, pc_x, pc_y, name) {
  library(ggplot2)
  
  gg.ready.pca.data.norm.log1p <- data.frame(DonorID = rownames(dataf), pca$x, age= dataf$Age, Gender = dataf$Gender)
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  
  pcx <- paste0("pc",pc_x)
  pcy <- paste0("pc",pc_y)
  
  ggplot_dynamic <- ggplot2::ggplot(data = gg.ready.pca.data.norm.log1p, aes(x=gg.ready.pca.data.norm.log1p[,pc_x + 1], y=gg.ready.pca.data.norm.log1p[,pc_y +1], color = age , group = Gender )) + 
    geom_point(aes(shape = Gender))+
    xlab(paste(pcx, " - " , var.pca[pc_x], "%", sep="")) + 
    ylab(paste(pcy," - " , var.pca[pc_y], "%", sep="")) +
    theme_bw() +
    ggtitle(paste("PCA plot of", pcx ," and ", pcy, "of", name )) +
    scale_color_gradient(low="blue", high="red", limits=c(42,101)) +
    scale_shape_manual(values=c( "F" = 16, 
                                 "M" = 17))
  
  return(ggplot_dynamic)}
