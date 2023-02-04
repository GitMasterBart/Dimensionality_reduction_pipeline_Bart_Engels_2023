library(ggpubr)
library(scales)
library(stats)
library(dplyr)
library(forcats)
library(viridis)
library(ggplot2)
library(ggsignif)
library(readxl)
library(MASS)
library(ggsignif)
library(tidyverse)
library(psych)
library(ggplot2)
library(stats)
library(ggpubr)

transpose_df <- function (df) {
  df <- as.data.frame(t(df))
  row.names(df) < df$X
  #colnames(df) <-df[1,]
  df$X <- NULL
  df <- sapply(df, function (x) as.integer(x))
  return(df[1:80])
}
gg_plot_clust_summary.total <- function(df, c1) 
{return(ggarrange(ggplot_symptomp_coherence(df, c1), 
                  ggplot_orthongalD_sex(c1), ggplot_orthongalD_age(c1),  
                  ggplot_symptomp_diagnosis.total(df,c1) + remove("y.text"), 
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2))}

gg_plot_clust_summary <- function(df, c1, title) 
{return(ggarrange(ggplot_symptomp_coherence(df, c1), 
                  ggplot_orthongalD_sex(c1), ggplot_orthongalD_age(c1),  
                  ggplot_orthongalD_diagnosis(data,c1) + remove("y.text"),
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2))}


ggplot_symptomp_diagnosis.total <- function(data, c){
  
  # Create a count matrix for the data
  count_matrix <- create_count_matrix(c, c$main_diagnosis)
  
  #count_matrix.full <- create_count_matrix(data, data$Main_diagnosis)
  
  #count_matrix <- merge(count_matrix, count_matrix.full, by = 2 )
  
  #count_matrix$Mean <- count_matrix$n.x/count_matrix$n.y
  
  count_matrix$fraction <- count_matrix$n / sum(count_matrix$n)
  
  count_matrix$perc <- count_matrix$fraction * 100
  
  
  return(ggplot(head(count_matrix[order(count_matrix$perc, decreasing = T),]), aes(x="", y=perc, fill=category))+
           geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
           ggtitle("") +
           theme(panel.background = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_text(hjust = 0.5, size = 8),
                 legend.title = element_text(size = 5), 
                 legend.key.size = unit(.2, 'cm')) +
           geom_text(aes(x=1.8, label=paste0(round(perc), "%")),
                     position = position_stack(vjust=0.5), size = 3))}

ggplot_symptomp_coherence <- function(df.thumb, clust) {
  
  # df subset df.thumb on clust1 data
  df.logn1 <- df.thumb
  df.Sumfreq.symptomps <- 
    data.frame(sum = 
                 colSums(df.logn1[row.names(df.thumb)
                                  %in% row.names(clust),][1:80]))
  
  df <- data.frame(row.names = 
                     head(row.names(df.Sumfreq.symptomps)
                          [order(df.Sumfreq.symptomps$sum, decreasing = T)],
                          n =15), sum =
                     head(df.Sumfreq.symptomps
                          [order(df.Sumfreq.symptomps$sum, decreasing = T),],
                          n =15) )
  
  # group the symptomps per domain
  c.groupings.freq <- function(v){
    
    Cognitive = c("Dementia", "Bradyphrenia", "Disinhibition",
                  "Loss_of_sympathy","Amnesia", "Apathy_inertia", 
                  "Aphasia", "Apraxia", "Confabulations", "Disinhibition",
                  "Executive_function_disorder", "Facade_behavior",
                  "Forgetfulness" , "Hyperorality", "Impaired_recognition",
                  "Impaired_recognition", "Imprinting_disturbances", 
                  "Language_impairment", "Loss_of_decorum" ,
                  "Memory_impairment", "Word_finding_problems" )
    
    Sensory = c("Hearing_problems", "Negative_sensory_symptoms", 
                "Constipation", "Olfactory_gustatory_dysfunction", 
                "Orthostatic_hypotension", "Positive_sensory_symptoms", 
                "Urinary_incontinence", "Urinary_problems_other", 
                "Visual_problems" )
    
    Psychiatric = c("Aggressive_behavior", "Agitation", "Anxiety", 
                    "Changed_moods_emotions", "Compulsive_behavior", 
                    "Confusion", "Day_night_rhythm_disturbances" ,
                    "Delirium", "Delusions", "Depressed_mood", 
                    "Disorientation", "Hallucinations", 
                    "Lack_of_insight",  "Mania", 
                    "Paranoia_suspiciousness", "Psychiatric_admissions", 
                    "Psychosis", "Restlessness", 
                    "Suicidal_ideation" , "Wandering" )
    
    Motor = c("Balance_problems","Bradykinesia", 
              "Decreased_motor_skills", "Dysarthria","Facial_masking", 
              "Fasciculations" , "Frequent_falls", "Hyperreflexia_and_oth_reflexes" 
              , "Loss_of_coordination", "Mobility_problems", 
              "Muscular_Weakness", "Nystagmus", 
              "Parkinsonism", "Rigidity", 
              "Spasticity", "Swallowing_problems_Dysphagia", 
              "Tremor", "Vertigo" )
    
    General = c("Admission_to_nursing_home", "Cachexia", 
                "Concentration_problems", "Day_care", 
                "Declined_deteriorated_health", "Fatigue",
                "Headache_migraine", "Help_in_ADL", 
                "Seizures", "Sleep_disturbances", 
                "Stress", "Vivid_dreaming", 
                "Weight_loss", "Communication_problems", 
                "Reduces_oral_intake")
    
    return(ifelse(row.names(v) %in% Cognitive , 
                  yes="Cognitive", 
                  no=ifelse(row.names(v) %in% Sensory, yes = "Sensory", 
                            no =ifelse(row.names(v) %in% Psychiatric,
                                       yes = "Psychiatric", 
                                       no = ifelse(row.names(v) %in% Motor, 
                                                   yes = "Motor", 
                                                   no=ifelse(row.names(v)
                                                             %in% General,
                                                             yes = "General", 
                                                             no =  "red"))))))}
  
  
  # plot Symptoms coherence 
  return (ggplot(data = df, 
                 aes(x = reorder(row.names(df),
                                 -abs(df$sum)), y =df$sum , fill =c.groupings.freq(df) )) +
            geom_col() +
            labs(x = "", y = "")+
            theme(axis.text.x = element_text(angle = 90, size = 5)) +
            scale_fill_manual(values = c("Cognitive" = "#E7BA52",
                                         "Sensory" = "#8CA252",
                                         "Psychiatric" = "#AD494A",
                                         "Motor"= "#393B79",
                                         "General" = "#A55194")) +
            labs(fill="Symptoms:") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  legend.text = element_text(size = 6), 
                  legend.title = element_text(size = 8), 
                  legend.key.size = unit(.5, 'cm')))}

ggplot_orthongalD_age <- function(data){
  return(ggplot(data = data, aes(x = data$age, y = smooth(..count..), fill = data$age, color = data$sex)) +
           geom_line(stat= "count")+
           labs(x = "", y = "", title = "Age of death frequency") +
           theme_bw()+
           labs(color="Sex:"))} 

# the frequency of the sexes in cluster 1
ggplot_orthongalD_sex <- function(data){ 
  
  
  return(ggplot(data = data, aes(x = data$sex, y = ..count.., fill = data$sex)) +
           geom_bar(stat = "count") +
           labs(x = "", y = "") +
           theme_bw()+
           labs(fill="Sex:") +
           geom_text(aes(label = ..count..), stat = "count", vjust = 1.5) )}


# the frequency of the sexes in clusters 
ggplot_orthongalD_sex.v2 <- function(clust, data){ 
  count_gender_m <- create_count_matrix(clust, clust$sex)
  print(count_gender_m)
  count_matrix.full <- create_count_matrix(data, data$Gender)
  print(count_matrix.full)
  count_gender_m <- merge(count_gender_m, count_matrix.full, by = "category" )
  
  count_gender_m$Mean <- count_gender_m$n.x/count_gender_m$n.y
  
  count_gender_m$fraction <- count_gender_m$Mean / sum(count_gender_m$Mean)
  
  count_gender_m$perc <- count_gender_m$fraction * 100
  
  return(ggplot(data = count_gender_m, aes(x = "" ,y = perc, fill = count_gender_m$sex)) + coord_polar("y", start=0) +
           geom_bar(stat = "identity") + coord_polar("y", start=0) +
           labs(x = "", y = "") +
           theme_bw()+
           labs(fill="Sex:") +
           theme(panel.background = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_text(hjust = 0.5, size = 8),
                 legend.title = element_text(size = 5), 
                 legend.key.size = unit(.2, 'cm')) +
           geom_text(aes(x=1.8, label=paste0(round(perc), "%")),
                     position = position_stack(vjust=0.5), size = 3)+
           scale_fill_manual(values = c("F" = "pink",
                                        "M" ="navey")))}


#ggplot_orthongalD_sex.v2(clust.1, df.thumb)


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

fa.cluster.ggplot.total <- function(fa, clusters, colculst, col) { 
  return(ggplot(fa, aes(x = fa$fa1, y= fa$fa2, 
                        color = as.character(clusters$cluster) ) ) + 
           geom_point() + 
           theme_bw() +
           xlab("Factor1") + 
           ylab("Factor2") +
           labs(color="Clusters:")) }


# the frequency of the diagnosis in cluster 1
ggplot_orthongalD_diagnosis.v2 <- function(data){
  maxi <- max(table(data$main_diagnosis)) 
  div.maxi <- (maxi/sum(maxi))
  space <- maxi + ( (maxi/100) * 5)
  return(ggplot(data = data, aes(x = data$main_diagnosis, 
                                 fill = data$main_diagnosis)) +
           geom_bar(stat = "count") +
           labs(x = "", y = "") +
           geom_text(aes(label = ..count.. ), stat = "count", vjust = -0.5,size = 2) +
           theme_bw()+
           ylim(0, space) + 
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
                                        "VD" = "red" ), name = "Diagnosis:")+ 
           theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                 legend.text = element_text(size = 6), 
                 legend.title = element_text(size = 8), 
                 legend.key.size = unit(.2, 'cm')))}


create_count_matrix <- function(data, group_var) {
  # Load the dplyr package
  library(dplyr)
  
  # Create a count matrix by grouping the data by the group variable and 
  # counting the number of rows within each group
  count_matrix <- data %>% group_by(!!group_var) %>% tally()
  count_matrix$category <- count_matrix$`<chr>`
  count_matrix$`<chr>` <- NULL
  # Return the count matrix
  return(count_matrix)
}
#ggplot_orthongalD_diagnosis(df.thumb,clust.1)
# the frequency of the diagnosis in cluster 1
ggplot_orthongalD_diagnosis <- function(data, c){
  
  # Create a count matrix for the data
  
  count_matrix <- create_count_matrix(c, c$main_diagnosis)
  
  #count_matrix.full <- create_count_matrix(data, data$Main_diagnosis)
  
  #count_matrix <- merge(count_matrix, count_matrix.full, by = 2 )
  
  #count_matrix$Mean <- count_matrix$n.x/count_matrix$n.y
  
  count_matrix$fraction <- count_matrix$n / sum(count_matrix$n)
  
  count_matrix$perc <- count_matrix$fraction * 100
  
  
  return(ggplot(head(count_matrix[order(count_matrix$perc, decreasing = T),]), aes(x="", y=perc, fill=category))+
           geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
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
           theme(panel.background = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.title = element_text(hjust = 0.5, size = 8),
                 legend.title = element_text(size = 5), 
                 legend.key.size = unit(.2, 'cm')) +
           geom_text(aes(x=1.8, label=paste0(round(perc), "%")),
                     position = position_stack(vjust=0.5), size = 3))}

biplot.fa <- function(fa, data, method, rotation, fa1, fa2) { return(
  ggplot(data = as.data.frame(fa$scores), 
         aes(x = fa$scores[,fa1], y = fa$scores[,fa2],
             color = data$Main_diagnosis)) + 
    geom_point() +
    theme_bw() +
    xlab("Factor1") + 
    ylab("Factor2") +
    ggtitle(paste(method,"Factor analysis, rotation:", rotation)) +
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
                                  "VD" = "red"
    ), name = "Diagnosis:") )}
