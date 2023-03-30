
stack_bar_plot <- function (cluster_set)
  # returns a stackbar plot with the distriubiton of the diagnosis
  # input: df cluster information
  # output: ggplot stackbarplot that repersents the diagnoses per cluster
  return (ggplot(cluster_set, aes(fill=main_diagnosis, y=abs(cluster), x=as.character(cluster))) +
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
                                        "VD" = "red",
                                        "PTSD, PSYCH" = "blue",
                                        "ASD, PSYCH" = "#E5FFCC",
                                        "DEPRI, PSYCH" = "#FF4500",
                                        "DEV" = "#00FF7F",
                                        "ADHD, PSYCH" = "#191970",
                                        "PSYCH" = "#FF00FF",
                                        "OCD, PSYCH" = "#6495ED",
                                        "NARCO, PSYCH" = "#D2691E",
                                        "PSYCH, DEPMA" =  "#DEB887"


           ), name = "Diagnosis:") + theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = .5,
          margin = margin(10, 0, 30, 0)
        ),
        plot.caption = element_text(
          size = 15,
          color = "grey40",
          hjust = .5,
          margin = margin(20, 0, 5, 0)
        ),
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "White", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10, color = "grey40"),
        legend.box.margin = margin(t = 30),
        legend.background = element_rect(
          color = "grey40",
          size = .3,
          fill = "grey95"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"),
        plot.margin = margin(rep(20, 4))
      ))

barclustercount <- function(df.clusts) { return( ggplot(df.clusts, aes(x=Var1, y=Freq, fill = Var1)) +
  # returns a barplot of the donor frequancy per cluster
  # input:
  # df.cluster: column with df of table df.clusts$cluster
  # output: GGplot with barplots frequancy donors per cluster
  geom_bar(stat = "identity") +
    theme_bw()+
    labs(tag = "B") +theme(plot.title = element_text(hjust = 0.5),
        plot.tag = element_text(size = 15),
        plot.tag.position = c(.95, .95)) +
    geom_text(aes(label =Freq ), vjust=-.9)+
    xlab("Clusters") +
  scale_fill_manual(values=c("#E66912", "#016367","#8CA252", "#D43649", "#9E3A14", "#2a9d8f", "#9e2a2b","#6E3562" , "red"), name = "Clusters"))}

age_distribution_graph <- function(df.clust.gen) {
  # creates a violin with boxplot for de age distribution
  # Input: needs a df that has the donors with and ages and linked cluster in a lonf format
  # return: ggplot violinplot with age distribution
  return (ggplot(df.clust.gen , aes(as.character(cluster), Age, fill = as.character(cluster))) +
  geom_violin() +
  geom_boxplot(width=0.06, color="grey", alpha=0.2) +
  scale_fill_manual(values=c("#E66912", "#016367","#8CA252", "#D43649", "#9E3A14", "#2a9d8f", "#9e2a2b","#6E3562" , "red")) +
  theme_bw() +

  theme(text = element_text(size = 20)) +
  labs(x="Cluster numbers", y=" Age (in Years)",  color = "Legend", fill = "clusters") +
  ggtitle("Age distribution for each cluster") +
   theme_classic())}

ggplot_symptomp_coherence <- function(df.thumb, clust) {
  # returns a ggbarplot that represents the signs/symptoms present per
  # cluster relative to the number of total signs/symptoms.
  # data_frame subset data_frame.thumb on clust1 data
  df.logn1 <- df.thumb
  Psychiatric = c("Aggressive_behavior", "Agitation", "Anxiety",
                    "Changed_moods_emotions", "Compulsive_behavior",
                    "Confusion", "Day_night_rhythm_disturbances" ,
                    "Delirium", "Delusions", "Depressed_mood",
                    "Disorientation", "Hallucinations",
                    "Lack_of_insight",  "Mania",
                    "Paranoia_suspiciousness", "Psychiatric_admissions",
                    "Psychosis", "Restlessness",
                    "Suicidal_ideation" , "Wandering" )


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

    Cognitive <- c("Dementia", "Bradyphrenia", "Disinhibition",
                   "Loss_of_sympathy", "Amnesia", "Apathy_inertia",
                   "Aphasia", "Apraxia", "Confabulations", "Disinhibition",
                   "Executive_function_disorder", "Facade_behavior",
                   "Forgetfulness" , "Hyperorality", "Impaired_recognition",
                   "Impaired_recognition", "Imprinting_disturbances",
                   "Language_impairment", "Loss_of_decorum" ,
                   "Memory_impairment", "Word_finding_problems" )

    Sensory <- c("Hearing_problems", "Negative_sensory_symptoms",
                 "Constipation", "Olfactory_gustatory_dysfunction",
                 "Orthostatic_hypotension", "Positive_sensory_symptoms",
                 "Urinary_incontinence", "Urinary_problems_other",
                 "Visual_problems" )

    Psychiatric <- c("Aggressive_behavior", "Agitation", "Anxiety",
                     "Changed_moods_emotions", "Compulsive_behavior",
                     "Confusion", "Day_night_rhythm_disturbances" ,
                     "Delirium", "Delusions", "Depressed_mood",
                     "Disorientation", "Hallucinations",
                     "Lack_of_insight", "Mania",
                     "Paranoia_suspiciousness", "Psychiatric_admissions",
                     "Psychosis", "Restlessness",
                     "Suicidal_ideation" , "Wandering" )

    Motor <- c("Balance_problems", "Bradykinesia",
               "Decreased_motor_skills", "Dysarthria", "Facial_masking",
               "Fasciculations" , "Frequent_falls", "Hyperreflexia_and_oth_reflexes"
              , "Loss_of_coordination", "Mobility_problems",
               "Muscular_Weakness", "Nystagmus",
               "Parkinsonism", "Rigidity",
               "Spasticity", "Swallowing_problems_Dysphagia",
               "Tremor", "Vertigo" )

    General <- c("Admission_to_nursing_home", "Cachexia",
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
                                 -abs(df$sum)), y =df$sum/sum(df.Sumfreq.symptomps$sum)*100 , fill =c.groupings.freq(df) )) +
            geom_col() +
            labs(x = "", y = "")+
            theme(axis.text.x = element_text(angle = 90, size = 9)) +
            scale_fill_manual(values = c("Cognitive" = "#E7BA52",
                                         "Sensory" = "#8CA252",
                                         "Psychiatric" = "#AD494A",
                                         "Motor"= "#393B79",
                                         "General" = "#A55194")) +
            coord_flip() +
            labs(fill="Symptoms:") +
            theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  legend.key.size = unit(.5, 'cm')))}
