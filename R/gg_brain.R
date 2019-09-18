#' @title Function to plot planes in `ggplot2` environment
#' @description The idea behind this script is to make something like wholebrain::plot.outlines(regi, TRUE) but in ggplot2
#' @param contour_list comes from `prep_data(regi)` function
#' @import tidyverse

#' @export
gg_brain <- function(contour_list, grayscale=TRUE, color=FALSE, fill=FALSE){


  # bind the contours
  df <- suppressWarnings(dplyr::bind_rows(contour_list, .id="image_file")) %>%
    mutate(base_file = basename(image_file)) %>%
    # scale with scaling factor
    mutate_at(vars("xrT", "yrT", "xlT", "ylT"),
              .funs = ~. * scale_factor)

  # TODO: when fill/color is passed inside aes() it's not interpreted as literal
  # see https://stackoverflow.com/questions/28206129/hexadecimal-colors-in-ggplot-not-colored-as-expected

  p1 <-  df %>%
      #  mutate(border_color = rep(regi$atlas$col, 68))%>%
      ggplot(aes(group=contour.ID, fill=color))+
      geom_polygon(aes(xrT, yrT), color="black")+
      geom_polygon(aes(xlT, ylT), color="black")+
      theme_void()+
      theme(panel.background = element_rect(fill="gray90"),
            legend.position="none")+
      scale_y_reverse()


  p2 <- df %>%
    #  mutate(border_color = rep(regi$atlas$col, 68))%>%
      ggplot(aes(group=contour.ID, color=color))+
      geom_polygon(aes(xrT, yrT))+
      geom_polygon(aes(xlT, ylT))+
      theme_void()+
      theme(panel.background = element_rect(fill="gray90"),
            legend.position="none")+
      scale_y_reverse()

  p3 <- df %>%
    ggplot(aes(group=contour.ID))+
    geom_path(aes(xrT, yrT),  color="black")+
    geom_path(aes(xlT, ylT), color="black")+
    theme_void()+
    theme(panel.background = element_rect(fill="gray90"),
          legend.position="none")+
    scale_y_reverse()


  #labels <- data.frame(base_file=unique(df$base_file),
  #                     label=unique(df$base_file))


  # TODO: CHANGE THINGS IN THE IF STATEMENTS SO THAT WE ONLY COMPUTE WHAT'S NEEDED

  if(color){
   print(p1 + facet_wrap(~base_file) )
  }
  if(fill){
    print(p2 + facet_wrap(~base_file))

  }
  if(grayscale){
    n_pages <- ceiling(length(unique(df$base_file)) / 9)

    for(i in 1:n_pages){
      print(p3 + ggforce::facet_wrap_paginate(~base_file, nrow = 3, ncol = 3,
                                     as.table = TRUE, strip.position = "top",
                                     page = i) +
      #        geom_label(data = labels, aes(label=label),
      #                   x = Inf, y = -Inf, hjust=1, vjust=0,
      #                   inherit.aes = FALSE) +
        NULL)

    }
  }

}




