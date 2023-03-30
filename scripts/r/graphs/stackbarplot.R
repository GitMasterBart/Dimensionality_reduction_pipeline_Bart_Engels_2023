subset <- c("MD", "BP", "SCHIZ", "PSYCH", "DEPRI, PSYCH", "DEV", "ASD, PSYCH", "ADHD, PSYCH", "PTSD, PSYCH" , "OCD, PSYCH","NARCO, PSYCH", "PSYCH, DEPMA")

stack_bar_plot <- function (cluster_set) return (ggplot(cluster_set, aes(fill=main_diagnosis, y=abs(cluster), x=as.character(cluster))) +
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