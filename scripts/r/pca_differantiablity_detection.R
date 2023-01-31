#!/usr/bin/env Rscript

df.violin.plot <- function(pca, df, pc) {
  pca.df <- data.frame(pca$x)
  list2 <- data.frame()
  for ( i in unique(df$Main_diagnosis)) {
    df.i <-  data.frame(text = i , value = pca.df[,pc][row.names(pca.df) %in% row.names(df[df$Main_diagnosis == i,])])
    list2 <- rbind(list2, df.i)
  }
  return(list2) }


list.pc.symptoom.corr <- function(pca, df){
  library("reshape2")
  l <- c()
for ( i in c(1:80)) {
  df.v <- df.violin.plot(pca, df,  i)
   main_diagnosis <- factor(df.v$text, labels = unique(df.v$text))
  res <- pairwise.wilcox.test(df.v$value, main_diagnosis, p.adjust.method = "none", exact=FALSE)
  res.1 <- sapply(data.frame(res$p.value), function(p) p.adjust(p, "bonferroni", n = (6640*6640)))
  df.no.na <- na.omit(melt(res.1))
  df.no.na.01 <- df.no.na[df.no.na$value < .05,]
  if (as.integer(dim(df.no.na.01)[1]) > 8){
      l <- append(l , i)
  }}
  return(l)}