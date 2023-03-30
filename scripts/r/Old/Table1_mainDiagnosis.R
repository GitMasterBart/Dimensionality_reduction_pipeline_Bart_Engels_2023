
library(dplyr)
library(plyr)
#clinical diagnosis annotated files

#pickel_csv_generalinformation 

Cognitive = c("Dementia", "Bradyphrenia", "Disinhibition", "Loss_of_sympathy","Amnesia", "Apathy_inertia", "Aphasia", "Apraxia", "Confabulations", "Disinhibition", "Executive_function_disorder", "Facade_behavior", "Forgetfulness" , "Hyperorality", "Impaired_recognition", "Impaired_recognition", "Imprinting_disturbances", "Language_impairment", "Loss_of_decorum" , "Memory_impairment", "Word_finding_problems" )

Sensory = c("Hearing_problems", "Negative_sensory_symptoms", "Constipation", "Olfactory_gustatory_dysfunction", "Orthostatic_hypotension", "Positive_sensory_symptoms", "Urinary_incontinence", "Urinary_problems_other", "Visual_problems" )

Psychiatric = c("Aggressive_behavior", "Agitation", "Anxiety", "Changed_moods_emotions", "Compulsive_behavior", "Confusion", "Day_night_rhythm_disturbances" , "Delirium", "Delusions", "Depressed_mood", "Disorientation", "Hallucinations", "Lack_of_insight",  "Mania", "Paranoia_suspiciousness", "Psychiatric_admissions", "Psychosis", "Restlessness", "Suicidal_ideation" , "Wandering" )

Motor = c("Balance_problems","Bradykinesia", "Decreased_motor_skills", "Dysarthria","Facial_masking", "Fasciculations" , "Frequent_falls", "Hyperreflexia_and_oth_reflexes" , "Loss_of_coordination", "Mobility_problems", "Muscular_Weakness", "Nystagmus", "Parkinsonism", "Rigidity", "Spasticity", "Swallowing_problems_Dysphagia", "Tremor", "Vertigo" )

General = c("Admission_to_nursing_home", "Cachexia", "Concentration_problems", "Day_care", "Declined_deteriorated_health", "Fatigue", "Headache_migraine", "Help_in_ADL", "Seizures", "Sleep_disturbances", "Stress", "Vivid_dreaming", "Weight_loss", "Communication_problems", "Reduces_oral_intake")



csv_generalinformation <- read.csv("~/Desktop/stage_umcg2022/data_umcg/parsed_data/currendly_used_04-11-2022/clinical_trajectories_dictionary_22_12_22.csv", header = T, sep = ",", row.names = 1)
row.names(csv_generalinformation)
csv_generalinformation$rownames <- row.names(csv_generalinformation)
csv_generalinformation$Domein <- (ifelse(row.names(csv_generalinformation) %in% Cognitive , yes="Cognitive",  no=ifelse(row.names(csv_generalinformation) %in% Sensory, yes = "Sensory", no =ifelse(row.names(csv_generalinformation) %in% Psychiatric, yes = "Psychiatric", no = ifelse(row.names(csv_generalinformation) %in% Motor, yes = "Motor", no=ifelse(row.names(csv_generalinformation) %in% General, yes = "General", no =  "red"))))))



csv_generalinformation <- csv_generalinformation %>% arrange(Domein)
csv_generalinformation <- data.frame(csv_generalinformation, row.names = csv_generalinformation$rownames)
csv_generalinformation$Domein <- NULL
csv_generalinformation$rownames <- NULL
csv_generalinformation_t <- data.frame(t(csv_generalinformation))
# deleting count Years 
csv_generalinformation_t$neurodeg_diag <- NULL
csv_generalinformation_t$neuropathological_diagnosis <- NULL
csv_generalinformation_t$sex <- NULL
csv_generalinformation_t$age_at_death <- NULL

# read main diagnoses linked to donor_id
main_diagnose_linked_id <- read.csv("~/Desktop/stage_umcg2022/data_umcg/parsed_data/currendly_used_04-11-2022/General_information_21-12-2022.xlsx - Sheet1.csv", )

main_diagnose_linked_id <- data.frame(DonorID = main_diagnose_linked_id$DonorID, Main_diagnosis = main_diagnose_linked_id$Main_diagnosis, Gender= main_diagnose_linked_id$Gender , Age = main_diagnose_linked_id$Age )

main_diagnose_list <- read.csv("~/Desktop/stage_umcg2022/data_umcg/MainDiagnosis_categories_and_metadata_NHB - 09092022.xlsx - Categories.csv", )[3:4]


## main_diagnose donorID convert
#typeof(csv_generalinformation_t$DonorID)
main_diagnose_linked_id$DonorID <- gsub("-", "../..", main_diagnose_linked_id$DonorID)
main_diagnose_linked_id$DonorID <- gsub(" ", "../..", main_diagnose_linked_id$DonorID)
#head(main_diagnose_linked_id)


#typeof(csv_generalinformation_t$DonorID)
csv_generalinformation_t$DonorID <- row.names(csv_generalinformation_t)
row.names(csv_generalinformation_t) <- csv_generalinformation_t$DonorID
#head(csv_generalinformation_t)



## merge general information with main_diagnoses_linked_id
csv_generalinformation_t$DonorID <- row.names(csv_generalinformation_t)

csv_generalinformation_t_main_diagnose <- merge(csv_generalinformation_t, main_diagnose_linked_id, by="DonorID")
row.names(csv_generalinformation_t_main_diagnose) <- csv_generalinformation_t_main_diagnose$DonorID 
csv_generalinformation_t_main_diagnose$DonorID<- NULL
#csv_generalinformation_t_main_diagnose$Main_diagnosis
csv_generalinformation_t_main_diagnose$Main_diagnosis <- gsub(']', "", csv_generalinformation_t_main_diagnose$Main_diagnosis )
csv_generalinformation_t_main_diagnose$Main_diagnosis <- gsub('\\[', "", csv_generalinformation_t_main_diagnose$Main_diagnosis )
csv_generalinformation_t_main_diagnose$Main_diagnosis <- gsub("\\'", "", csv_generalinformation_t_main_diagnose$Main_diagnosis )

csv_generalinformation_t_main_diagnose$Categories <- csv_generalinformation_t_main_diagnose$Main_diagnosis
#head(csv_generalinformation_t_main_diagnose$Categories)

#head(main_diagnose_list)
#head(csv_generalinformation_t_main_diagnose)


#csv_generalinformation_t_main_diagnose <- merge(csv_generalinformation_t_main_diagnose, main_diagnose_list, by="Categories" )
csv_generalinformation_t_main_diagnose$Categories <- NULL


#csv_generalinformation_t_main_diagnose$Main_diagnosis[csv_generalinformation_t_main_diagnose$Main_diagnosis == "SCHIZ, PSYCH"]



old_count <- n_distinct(csv_generalinformation_t_main_diagnose$Main_diagnosis)

for ( i in diseases.trans.dict$keys) {
  typeof(diseases.trans.dict$keys)
  csv_generalinformation_t_main_diagnose$Main_diagnosis[csv_generalinformation_t_main_diagnose$Main_diagnosis == i] <- diseases.trans.dict$get(i)
}


#Con_set <- subset(csv_generalinformation_t_main_diagnose, Main_diagnosis == "CON")
#AD_set <- subset(csv_generalinformation_t_main_diagnose, Main_diagnosis == "AD")
#ad_con_set <- subset(csv_generalinformation_t_main_diagnose, Main_diagnosis %in% c("CON","AD"))
#norm_log1p  <- log1p(csv_generalinformation_t_main_diagnose[1:8])

#csv_generalinformation_t_main_diagnose <- subset(csv_generalinformation_t_main_diagnose, Main_diagnosis %in% c('CON',"AD","PD", "PDD", "DLB", "VD", "FTD", "MND","PSP", "ATAXIA", "MS", "MSA", "MD", "BP", "SCHIZ"))


new_count <-n_distinct(csv_generalinformation_t_main_diagnose$Main_diagnosis)

total_removed <- old_count - new_count

print(paste("Shaved values: ", total_removed))

#row.names(csv_generalinformation_t_main_diagnose) <- csv_generalinformation_t_main_diagnose$DonorID
csv_generalinformation_t_main_diagnose$year_before_death <- NULL

write.csv(csv_generalinformation_t_main_diagnose,"~/Desktop/stage_umcg2022/data_umcg/parsed_data/currendly_used_04-11-2022/clincical_trajecotires_data_thumb_04-01-2022.total.csv", row.names = T)
