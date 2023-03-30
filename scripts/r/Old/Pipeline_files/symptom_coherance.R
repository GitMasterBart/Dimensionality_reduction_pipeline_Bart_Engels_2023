#!/usr/bin/env Rscript
library(plyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(pals)
library(stringr)
source(paste("/Users/bengels/Desktop/stage_umcg2022/scripts", "/Dimensionality_reduction_pipeline_Bart_Engels_2023/scripts/r/file_names.R", sep = ""))


df.clusts <- read.csv(paste(ROOT_FOLDER, "/Output/cluster_information.csv", sep = ""), row.names = 1)
df.gen <- read.csv(paste(ROOT_FOLDER, "/Input/General_information_13-07-2022_FULL.csv", sep = ""), row.names = 1)
df.summed <-  data.frame(t(read.csv(paste(ROOT_FOLDER, "/Input/summed_data_pickle.csv", sep = ""), row.names = 1))[,1:80])
typeof(df.summed)
df.summed <- data.frame(sapply(df.summed, function (x) as.integer(x)), row.names = row.names(df.summed))

change_NBB_Id <- function(df) {
  row.names(df) <- gsub(" ", ".", row.names(df))
  row.names(df) <- gsub("-", ".", row.names(df))
  return (df)
}
df.clusts <- change_NBB_Id(df.clusts)
df.gen <- change_NBB_Id(df.gen)

df.clust.summed <- merge(df.clusts, df.summed, by = 0, row.names = 0)
row.names(df.clust.summed) <- df.clust.summed$Row.names
df.clust.summed$Row.names <- NULL

df.clust.gen <- merge(df.gen, df.clusts, by = 0, row.names = 0)
# c("#E66912", "#016367","#8CA252", "#D43649", "#9E3A14", "#2a9d8f", "#9e2a2b","#6E3562" )

df.gen[df.gen$Age == 0,]
ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "Age_distribuiton_per_cluster",".png", sep = ""), ggplot(df.clust.gen , aes(as.character(cluster), Age, fill = as.character(cluster))) +
  geom_violin() +
  geom_boxplot(width=0.06, color="grey", alpha=0.2) +
  scale_fill_manual(values=as.vector(cols25(25) )) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x="Cluster numbers", y=" Age (in Years)",  color = "Legend", fill = "clusters") +
  ggtitle("Age distribution for each cluster") ,width = 20, height =20 , dpi = 150, units = "in", device='png'

)




specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)


# df.clust.gen %>% get(Date_and_time_of_death)

df.clust.gen$date <- as.Date(df.clust.gen$Date_and_time_of_death, format= "%d/%m/%Y")

df.clust.gen$dateY <- str_extract(df.clust.gen$date,  regex("(19[89][0-9]|20[0-4][0-9]|2050)"))

ggplot(df.clust.gen, aes(cluster,as.integer(dateY), fill=as.character(cluster) )) +
    geom_violin()

df <- data.frame()
df.t.subset <- data.frame()
for (i in 1:length(unique(df.clust.summed$cluster))){

col_sums <- data.frame(freq = colSums(df.clust.summed[df.clust.summed$cluster == i,][(dim(df.clusts)[2] + 1): (80+(dim(df.clusts)[2]))] ), cluster = i)
col_sums$symptom <- row.names(col_sums)
col_sums$sum <- sum(df.clust.summed[df.clust.summed$cluster == i,][(dim(df.clusts)[2] + 1):(dim(df.clusts)[2] + 80)])
col_sums$frac <- ( dim(df.clust.summed[df.clust.summed$cluster == i,])[1]) * col_sums$freq
col_sums$perc <- round(col_sums$freq / col_sums$sum * 100, 2)
df <- rbind(df, head(col_sums[order(col_sums$freq, decreasing = T),], n =5))
df.t.subset <- rbind(df.t.subset, head(col_sums[order(col_sums$frac, decreasing = T),], n =(dim(df.clusts)[2] + 80)))}

unique(df.t.subset$symptom)
ggsave(paste(ROOT_FOLDER ,"/Output/img/clusts/", "bartack_plot_symptoms",".png", sep = ""), df %>% ggplot( aes(fill=symptom, y=perc, x=as.character(cluster)))  +
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
  scale_fill_manual(values=as.vector(polychrome(36)))+
    ggtitle("") +
    theme_ipsum() +
    xlab(""))

  # data_frame.t <- rbind(data_frame.t, head(data.frame(row.names = paste("cluster",i), t(data.frame(perc =  round(colSums(data_frame.clust.summed[data_frame.clust.summed$cluster == i,][(dim(data_frame.clusts)[2] + 1):(dim(data_frame.clusts)[2] + 80)] ) / sum(data_frame.clust.summed[data_frame.clust.summed$cluster == i,][(dim(data_frame.clusts)[2] + 1):(dim(data_frame.clusts)[2] + 80)]) * 100, 2)))),n =5))

df.t <- data.frame()
for (i in 1:length(unique(df.clust.summed$cluster))){
  # print( (sum(data_frame.clust.summed[data_frame.clust.summed$cluster == i,][(dim(data_frame.clusts)[2] + 1):(dim(data_frame.clusts)[2] + 80)]) /
  #   colSums(data_frame.clust.summed[data_frame.clust.summed$cluster == i,][(dim(data_frame.clusts)[2] + 1):(dim(data_frame.clusts)[2] + 80)])[1] ) / 100 )
  print(sum(df.clust.summed[df.clust.summed$cluster == i,][(dim(df.clusts)[2] + 1):(dim(df.clusts)[2] + 80)]))

  # frac = ( (totalsymptom / group_size) / 100 ) * symptoms
  df.t <- rbind(df.t, data.frame(row.names = paste("cluster",i),
                                      t(data.frame(frac = (((sum(df.clust.summed[df.clust.summed$cluster == i,][(dim(df.clusts)[2] + 1):(dim(df.clusts)[2] + 80)]) /
                                        dim(df.clust.summed[df.clust.summed$cluster == i,])[1] ) / 100) )*
                                                                     colSums(df.clust.summed[df.clust.summed$cluster == i,][(dim(df.clusts)[2] + 1):80] )))))
}
df.t <- rbind(rep(max(df.t),75) , rep(0,75) , df.t)


df.t[,colnames(df.t) %in% head(row.names(df), n = 6)]


# pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome,
#   stepped, tol, watlington,
#   show.names=FALSE)

library(fmsb)

# Create data: note in High school for several students

unique(df.t.subset$symptom)

# head(unique(data_frame.t.subset$symptom), n = 20)
# Executive_function_disorder


pylist_psych <- c('Changed_behavior_personality', 'Compulsive_behavior', 'Aggressive_behavior', 'Agitation', 'Anxiety', 'Changed_moods_emotions', 'Depressed_mood', 'Mania', 'Restlessness', 'Disorientation', 'Lack_of_insight', 'Wandering', 'Confusion', 'Day_night_rhythm_disturbances', 'Delirium', 'Delusions', 'Hallucinations', 'Paranoia_suspiciousness', 'Psychosis', 'Psychiatric_admissions', 'Suicidal_ideation')
cog_gen_list <- c('Amnesia', 'Dementia', 'Confabulations', 'Dementia', 'Forgetfulness', 'Head_turning_sign', 'Impaired_recognition', 'Imprinting_disturbances', 'Memory_impairment', 'Apathy_inertia', 'Bradyphrenia', 'Disinhibition', 'Frontal_release_signs', 'Hyperorality', 'Loss_of_decorum', 'Loss_of_sympathy', 'Aphasia', 'Apraxia', 'Fatigue', 'Impaired_comprehension', 'Apathy_inertia', 'Executive_function_disorder', 'Language_impairment', 'Word_finding_problems', 'Facade_behavior', 'Cachexia', 'Day_care', 'Help_in_ADL', 'Admission_to_nursing_home', 'Declined_deteriorated_health', 'Reduces_oral_intake', 'Communication_problems', 'Concentration_problems', 'Executive_function_disorder', 'Headache_migraine', 'Seizures', 'Sleep_disturbances', 'Stress', 'Vivid_dreaming', 'Weight_loss')
cog_symp_list <- c('Amnesia', 'Dementia', 'Confabulations', 'Dementia', 'Forgetfulness', 'Head_turning_sign', 'Impaired_recognition', 'Imprinting_disturbances', 'Memory_impairment', 'Apathy_inertia', 'Bradyphrenia', 'Disinhibition', 'Frontal_release_signs', 'Hyperorality', 'Loss_of_decorum', 'Loss_of_sympathy', 'Aphasia', 'Apraxia', 'Impaired_comprehension', 'Apathy_inertia', 'Language_impairment', 'Word_finding_problems', 'Facade_behavior')
# head(unique(data_frame.t.subset$symptom), n = 20)
length(unique(df.t.subset$symptom))
# append(cog_gen_list, pylist_psych)
s <- subset(df.t.subset, df.t.subset$symptom %in%  append(cog_gen_list, pylist_psych))
unique(s$symptom )
# head(unique(s$symptom ), n = 20)
ee  <- df.t[,colnames(df.t) %in% head(unique(s$symptom ), n = 20)]
# plot with default options:
png(filename =paste(ROOT_FOLDER ,"/Output/img/clusts/", "symptom_coherance_clust",".png", sep = ""), width = 2000, height = 1000, units = 'px', res = 130)
#length(unique(data_frame.clust.summed$cluster))
  print(ee[c(1, 2, i+2), ])
radarchart( ee, , axistype=1 ,
    #custom polygon
    pcol=cols25(12)  , pfcol=adjustcolor(cols25(12) , alpha.f = 0.1) , plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c(" "," "," "," "," "), cglwd=0.6,
    #custom labels
    vlcex=0.5,palcex = 20
    # title
    )
leg.titles <- c("BP")
legend(x=1.5, y=1, legend = row.names(ee[-c(1,2),]), bty = "n", pch=15 , col=  adjustcolor(cols25(12), alpha.f = 0.3) , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()



unique(df.t.subset$symptom)

# plot with default options:
png(filename =paste(ROOT_FOLDER ,"/Output/img/clusts/", "symptom_coherance_clust_1_6",".png", sep = ""), width = 2000, height = 1000, units = 'px', res = 130)
#length(unique(data_frame.clust.summed$cluster))
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,3))
for(i in 7:12){
radarchart( ee[c(1, 2, i+2), ], , axistype=0.9 ,
    #custom polygon
    pcol=alphabet2(12)[i]  , pfcol=adjustcolor(alphabet(12)[i] , alpha.f = 0.1) , plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=2, axislabcol="grey", caxislabels=c(" "," "," "," "," "), cglwd=0.3,
    #custom labels
    vlcex=0.9,
    # title
    title = row.names(ee)[i + 2]
    )}
par(op)
# legend(x=1.5, y=1, legend = row.names(ee[-c(1,2),]), bty = "n", pch=15 , col=  adjustcolor(alphabet2(12), alpha.f = 0.3) , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()

unique(df.t.subset$symptom)

# plot with default options:
png(filename =paste(ROOT_FOLDER ,"/Output/img/clusts/", "symptom_coherance_clust_7_8",".png", sep = ""), width = 2000, height = 1000, units = 'px', res = 130)
#length(unique(data_frame.clust.summed$cluster))
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,3))
for(i in 1:6){
  print(ee[c(1, 2, i+2), ])
radarchart( ee[c(1, 2, i+2), ], , axistype=1 ,
    #custom polygon
    pcol=alphabet2(12)[i]  , pfcol=adjustcolor(alphabet(12)[i] , alpha.f = 0.1) , plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c(" "," "," "," "," "), cglwd=0.3,
    #custom labels
    vlcex=0.8,
    # title
    title = row.names(ee)[i + 2]
    )}
par(op)
# legend(x=1.5, y=1, legend = row.names(ee[-c(1,2),]), bty = "n", pch=15 , col=  adjustcolor(alphabet2(12), alpha.f = 0.3) , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()



png(filename =paste(ROOT_FOLDER ,"/Output/img/clusts/", "total_1_4",".png", sep = ""), width = 2000, height = 1000, units = 'px', res = 130)
#length(unique(data_frame.clust.summed$cluster))

radarchart( ee[1:6,], , axistype=1 ,
    #custom polygon
    pcol=cols25(12)  , pfcol=adjustcolor(cols25(12) , alpha.f = 0.1) , plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c(" "," "," "," "," "), cglwd=0.4,
    #custom labels
    vlcex=0.6,
    # title

    )

legend(x=1.5, y=1, legend = row.names(ee[3:7,]), bty = "n", pch=15 , col=  adjustcolor(cols25(12), alpha.f = 0.3) , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()

png(filename =paste(ROOT_FOLDER ,"/Output/img/clusts/", "total_5_8",".png", sep = ""), width = 2000, height = 1000, units = 'px', res = 130)
#length(unique(data_frame.clust.summed$cluster))

radarchart( ee[-c(3:6),], , axistype=1 ,
    #custom polygon
    pcol=cols25(12)  , pfcol=adjustcolor(cols25(12) , alpha.f = 0.1) , plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c(" "," "," "," "," "), cglwd=0.3,
    #custom labels
    vlcex=0.8,
    # title

    )

legend(x=1.5, y=1, legend = row.names(ee[c(7:10),]), bty = "n", pch=15 , col=  adjustcolor(cols25(12), alpha.f = 0.3) , text.col = "grey", cex=1.2, pt.cex=3)

dev.off()

