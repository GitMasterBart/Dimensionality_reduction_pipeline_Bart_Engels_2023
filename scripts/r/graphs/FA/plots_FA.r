biplot.fa <- function(fa, data, method, rotation, fa1, fa2) {
    # bi plot returns a scatter plot for the fa's
    # input:
    # Fa: output of fa/fa.sapa function,
    # data: df that is the output form the pca.
    # method: is the name of fa method
    # rotation: name of rotation technique
    # fa1 and fa2: variable on x and y axsis
    return(
  ggplot(data = as.data.frame(fa$scores),
         aes(x = fa$scores[,fa1], y = fa$scores[,fa2],
             color = data$Main_diagnosis)) +
    geom_point() +
    theme_bw() +
    xlab(paste("Factor", fa1)) +
    ylab(paste("Factor", fa2)) +
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

