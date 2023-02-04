


linegraph_temporal_data <- function( folder, clusts, end.year) {
  library(NbClust)
library(ggplot2)
library(ggstream)
library(colorspace)
library(tidyverse)

       for (i in unique(df_char_vis$symptom)[1:80]){
         levels <- c(i)
  # end.year <- 40 #max(df_char_vis$Year)
  # clusts <-  c("clust1", "clust5", "clust7") #colnames(df.info)


    create_df_best_chars <- function(symptom_list){
      df_best_chars <- tibble(
        rank = 1:length(symptom_list),
        char_popular = symptom_list)
      return(df_best_chars)
    }
    pal <- c(
      "#8CA252", lighten("#8CA252", .25, space = "HLS"),
      "#809c13", lighten("#809c13", .2, space = "HLS"),
      "#607c3c", lighten("#607c3c", .2, space = "HLS")

    )

    pal <- c(
      "#FFB400", lighten("#8CA252", .25, space = "HLS"),
      "#C20008", lighten("#6E3562", .2, space = "HLS"),
      "#13AFEF", lighten("#595A52", .25, space = "HLS"),
      "#8E038E", lighten("#FBCE3A", .2, space = "HLS"),
      "#595A52", lighten("#8390fa", .15, space = "HLS"),
      "#008000", lighten("#6495ED", .25, space = "HLS"),
      "#D2691E", lighten("#00cecb", .2, space = "HLS"),
      "#FF4500", lighten("#d1495b", .2, space = "HLS")

    )
    # df_char_vis <- df_char_vis %>%
    #   relocate(clust2, .before = clust4 )


    df <- df_char_vis[df_char_vis$symptom %in% levels,] %>%
      filter(Year < end.year)



    df_smooth <- df %>%
      group_by(symptom)  %>%
      slice(1:4) %>%
      mutate(
        Year = c(
          min(df$Year) - 2,
          min(df$Year) - .5,
          max(df$Year) + .5,
          max(df$Year) + 2
        ),
        clust1 = c(0, .001, .001, 0),
        clust2 = c(0, .001, .001, 0),
        clust3 = c(0, .001, .001, 0),
        clust4 = c(0, .001, .001, 0),
        clust5 = c(0, .001, .001, 0)
      )



    df_best_stream_fct <- df %>%
      bind_rows(df) %>%
      mutate(

        char_costume =  factor(symptom, levels = levels) ,


        #char_costume = fct_reorder(char_costume, rank)
      ) %>%
      pivot_longer(
        cols = clusts,
        names_to = "parameter",
        values_to = "value"
      ) #%>%
    #mutate(parameter = factor(parameter, levels = levels))






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
        #axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey88", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 15, color = "grey40"),
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
        fill = "grey88"
      ) +
      annotate(
        "rect",
        xmin = end.year, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "grey88"
      ) +
      scale_x_continuous(
        limits = c(0, NA),
        breaks = c(0, seq(0, end.year, by = 10), end.year),
        labels = glue::glue("Year\n{c(0, seq(0, {end.year}, by = 10), -{end.year})}"),
        position = "top"
      ) +
      # Customize labels of the horizontal axis
      #scale_y_continuous(expand = c(.03, .03)) +
      # This clip="off" is very important. It allows to have annotations anywhere
      # in the plot, no matter they are not within the extent of
      # the corresponding panel.
       coord_cartesian(clip = "off") #+
      #scale_x_reverse()


    g <- g +
      labs(
        title = paste("Symptom: ", gsub("_", " ", levels)),
        caption = "Made by • Bart Engels"
      )

  g

  ggsave(paste(ROOT_FOLDER ,"/Output/img/", folder, "/" , levels,".png", sep = ""), g ,width = 24, height = 9)
}}