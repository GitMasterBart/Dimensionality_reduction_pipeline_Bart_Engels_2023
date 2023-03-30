#!/usr/bin/env Rscript


linegraph_temporal_data <- function( folder, clusts, end.year, direct) {
    #  this function returns a line diagram that
    #
    #
    library(NbClust)
    library(ggplot2)
    library(ggstream)
    library(colorspace)
    library(tidyverse)
    library(RColorBrewer)


       for (i in unique(df_char_vis$symptom)[1:80]){
         levels <- c(i)

    create_df_best_chars <- function(symptom_list){
      df_best_chars <- tibble(
        rank = 1:length(symptom_list),
        char_popular = symptom_list)
      return(df_best_chars)
    }

  pal <- c("#E66912", "#016367","#8CA252", "#D43649", "#9E3A14", "#2a9d8f", "#9e2a2b","#6E3562", "red" )

    df <- df_char_vis[df_char_vis$symptom %in% levels,] %>%
      filter(Year < end.year)

    df_best_stream_fct <- df %>%
      bind_rows(df) %>%
      mutate(
        char_costume =  factor(symptom, levels = levels) ,

      ) %>%
      pivot_longer(
        cols = clusts,
        names_to = "parameter",
        values_to = "value"
      ) #%>%

    g <-df_best_stream_fct %>%
      ggplot(aes(Year, value,
                 color = parameter,
                  fill = parameter)) +
      geom_line() +
      geom_line(
        geom = "polygon",
        bw = .65,
        size = 0
      ) +
      scale_color_manual(
        expand = c(0, 0),
        values = pal
      ) +
      scale_fill_manual(
        values = pal
    )+
      theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = .5,
          margin = margin(10, 0, 30, 0)
        ),
        plot.caption = element_text(
          size = 9,
          color = "grey40",
          hjust = .5,
          margin = margin(20, 0, 5, 0)
        ),
        axis.text = element_text(size= 30),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "White", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 30, color = "grey40"),
        legend.box.margin = margin(t = 30),
        legend.background = element_rect(
          color = "grey40",
          size = .3,
          fill = "grey95"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"),
        plot.margin = margin(rep(40, 8))
      )

    g <- g + geom_vline(
        data = tibble(x = c(end.year, seq(end.year, 0, by = -5), 0)),
        aes(xintercept = x),
        inherit.aes = FALSE,
        color = "grey88",
        size = .5,
        linetype = "dotted"
      ) +
      annotate(
        "rect",
        xmin = -Inf, xmax = 0,
        ymin = -Inf, ymax = Inf,
        fill = "white"
      ) +
      annotate(
        "rect",
        xmin = end.year, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "white"
      ) +
      scale_x_continuous(
        limits = c(0, NA),
        breaks = c(0, seq(0, end.year, by = 10), end.year),
        labels = glue::glue("Year\n{c(0, seq(0, {end.year}, by = 10), {end.year})}"),
        position = "top"
      ) + scale_x_continuous(
        limits = c(0, NA),
        breaks = c(0, seq(0, end.year, by = 10), end.year),
        labels = glue::glue("Year\n{c(0, seq(0, {end.year}, by = 10), {end.year})}"),
        position = "top"
      ) +
      scale_x_reverse()


    g <- g +
      labs(
        title = paste("Symptom: ", gsub("_", " ", levels)),
        caption = "Made by • Bart Engels"
      )

  g

  ggsave(paste(ROOT_FOLDER ,"/Output/clusts/img/temporal_patterns/", folder, "/" , levels,".png", sep = ""), g ,width = 24, height = 9)
}
}
