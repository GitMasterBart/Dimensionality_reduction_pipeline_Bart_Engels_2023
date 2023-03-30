
# group pos and neg
c.pc <- function(pca,n){
  print(pca$rotation[,n])
  return(ifelse(pca$rotation[,n] > 0, yes="green2", no="red2"))
  
}
# groups domains
c.groupings <- function(pca,n){
  
  Cognitive = c("Dementia", "Bradyphrenia", 
                "Disinhibition", "Loss_of_sympathy",
                "Amnesia", "Apathy_inertia", 
                "Aphasia", "Apraxia", 
                "Confabulations", "Disinhibition", 
                "Executive_function_disorder", "Facade_behavior", 
                "Forgetfulness" , "Hyperorality", 
                "Impaired_recognition", "Imprinting_disturbances", 
                "Language_impairment", "Loss_of_decorum" , 
                "Memory_impairment", "Word_finding_problems" )
  
  Sensory = c("Hearing_problems", "Negative_sensory_symptoms", 
              "Constipation", "Olfactory_gustatory_dysfunction", 
              "Orthostatic_hypotension", "Positive_sensory_symptoms", 
              "Urinary_incontinence", "Urinary_problems_other", "Visual_problems" )
  
  Psychiatric = c("Aggressive_behavior", "Agitation", 
                  "Anxiety", "Changed_moods_emotions", 
                  "Compulsive_behavior", "Confusion", 
                  "Day_night_rhythm_disturbances" , "Delirium", 
                  "Delusions", "Depressed_mood", 
                  "Disorientation", "Hallucinations", 
                  "Lack_of_insight",  "Mania", 
                  "Paranoia_suspiciousness", "Psychiatric_admissions", 
                  "Psychosis", "Restlessness", 
                  "Suicidal_ideation" , "Wandering" )
  
  Motor = c("Balance_problems","Bradykinesia", 
            "Decreased_motor_skills", "Dysarthria",
            "Facial_masking", "Fasciculations" , 
            "Frequent_falls", "Hyperreflexia_and_oth_reflexes" , 
            "Loss_of_coordination", "Mobility_problems", 
            "Muscular_Weakness", "Nystagmus", 
            "Parkinsonism","Rigidity",
            "Swallowing_problems_Dysphagia", 
            "Tremor", "Vertigo",
            "Spasticity")
  
  General = c("Admission_to_nursing_home", "Cachexia", 
              "Concentration_problems", "Day_care", 
              "Declined_deteriorated_health", "Fatigue", 
              "Headache_migraine", "Help_in_ADL",
              "Seizures", "Sleep_disturbances", 
              "Stress", "Vivid_dreaming", 
              "Weight_loss", "Communication_problems", 
              "Reduces_oral_intake")
 
  
  return(ifelse(row.names(pca$rotation) %in% Cognitive , yes="#E7BA52",  no=ifelse(row.names(pca$rotation) %in% Sensory, yes = "#8CA252", no =ifelse(row.names(pca$rotation) %in% Psychiatric, yes = "#AD494A", no = ifelse(row.names(pca$rotation) %in% Motor, yes = "#393B79", no=ifelse(row.names(pca$rotation) %in% General, yes = "#A55194", no =  "red"))))))}

n.pc <- function(pca,n){
  return(ifelse(pca$rotation[,1] > 0, -0.01, pca$rotation[,1]-0.01))}
# plot loadings pos and negative
plot_loadings_pos_neg <- function(pca, pc) {
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100, 1)
  b2<-   barplot(pca$rotation[,pc], main= paste("PC", pc, "Loadings Plot", var.pca), col=c.pc(pca,pc), las=2, axisnames=FALSE)
  abline(h=0)
  # Add variable names
  text(x=b2, y=ifelse(pca$rotation[,pc] > 0, -0.01, pca$rotation[,pc]-0.01), 
       labels=names(pca$rotation[,pc]), adj=1, srt=90, xpd=NA, cex = 0.45)
}
# plots the loading's en shows group per symptom






plot_loadings <- function(pca, pc, title, scale) {
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100)
  b2 <- barplot(pca$rotation[,pc], main= paste("PC", pc, title,"| Variance:", var.pca[pc], "%" ), col=c.groupings(pca,pc), las=2, axisnames=FALSE)
  abline(h=0)
  # Add variable names
  text(x=b2, y=ifelse(pca$rotation[,pc] > 0, -0.01, pca$rotation[,pc]-0.01), 
       labels=names(pca$rotation[,pc]), adj=1, srt=90, xpd=NA, cex = 0.45)
  legend("topleft", legend=c("Cognitive", "Sensory", "Psychiatric", "Motor", "General"),
         col=c("#E7BA52", "#8CA252","#AD494A","#393B79" ,"#A55194"), lty=1, cex=0.5 * scale)
}


plot_loadings_loc <- function(pca, pc, title, scale, loc) {
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100)
  b2 <- barplot(pca$rotation[,pc], main= paste("PC", pc, title,"| Variance:", var.pca[pc], "%" ), col=c.groupings(pca,pc), las=2, axisnames=FALSE)
  abline(h=0)
  # Add variable names
  text(x=b2, y=ifelse(pca$rotation[,pc] > 0, -0.01, pca$rotation[,pc]-0.01), 
       labels=names(pca$rotation[,pc]), adj=1, srt=90, xpd=NA, cex = 0.45)
  legend(loc, legend=c("Cognitive", "Sensory", "Psychiatric", "Motor", "General"),
         col=c("#E7BA52", "#8CA252","#AD494A","#393B79" ,"#A55194"), lty=1, cex=0.5 * scale)
}






c.groupings_fa <- function(pca,n){
  
  return(ifelse(pca$loadings[,n] > 0, yes="green2", no="red2"))}

plot_loadings_fa <- function(pca, fa, title, scale) {
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100)
  b2<-   barplot(pca$loadings[,fa], main= paste("Loadings", "Factor", fa, title), col=c.groupings_fa(pca,fa), las=2, axisnames=FALSE)
  abline(h=0)
  # Add variable names
  text(x=b2, y=ifelse(pca$loadings[,fa] > 0, -0.01, pca$loadings[,fa]-0.01), 
       labels=names(pca$loadings[,fa]), adj=1, srt=90, xpd=NA, cex = 0.45)
  legend("topleft", legend=c("Cognitive", "Sensory", "Psychiatric", "Motor", "General"),
         col=c("#E7BA52", "#8CA252","#AD494A","#393B79" ,"#A55194"), lty=1, cex=0.5 * scale)
}

scree_plot <- function(pca, title, col){
  var = pca$sdev^2 / sum(pca$sdev^2) *100
  barplot(head(var, n=10), pch = 1, ylab = "variance explaind" , 
          xlab = "Componends", main = paste(title))
  lines(var, col = col)
  points(var)
}

plot_loadings_sort <- function(pca, pc, title, scale) {
  pca.var <- pca$sdev^2
  var.pca <- round(pca.var/sum(pca.var)*100)
  b2 <- barplot(sort(pca$rotation[,pc], decreasing = T), main= paste("PC", pc, title,"| Variance:", var.pca[pc], "%" ), col=c.groupings(sort(pca),pc), las=2, axisnames=FALSE)
  abline(h=0)
  # Add variable names
  text(x=b2, y=ifelse(sort(pca$rotation[,pc], decreasing = T) > 0, -0.01, pca$rotation[,pc]-0.01), 
       labels=names(sort(pca$rotation[,pc], decreasing = T)), adj=1, srt=90, xpd=NA, cex = 0.45)
  legend("topleft", legend=c("Cognitive", "Sensory", "Psychiatric", "Motor", "General"),
         col=c("#E7BA52", "#8CA252","#AD494A","#393B79" ,"#A55194"), lty=1, cex=0.5 * scale)
}


