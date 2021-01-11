#' @title Function to plot planes in `ggplot2` environment
#' @description This function will make a table with the information
#'  we need to provide to the cropping function
#' @param regi object from any `registration` function(s)
#' @param ordered_filtered_list a list of the filters feed to
#'  the regi object in the same order (check names(ordered_filtered_list) matches the regi[[i]])
#' @import tidyverse

#' @export
#
prep_data <- function(regi, ordered_filter_list) {

  num_regis <- length(regi)

  if (num_regis == 0){ stop("must provide regi object with length >= 1")}

  # check lengths
  if(length(ordered_filter_list) >1 ){
    if(num_regis != length(ordered_filter_list)){
      stop(sprintf("Unequal length of arguments.
                 \nlength of regi is %s & length of ordered_filter_list is %s.",
                   num_regis, length(ordered_filter_list)))
    }

  } else {

    ordered_filter_list <- rep(ordered_filter_list, num_regis)

  }

  # preallocate list
  output_list <- vector(mode = "list", length = num_regis)
  # names should come from the ordered filter list
  # TODO: we can also decide to name the regi object before...
  names(output_list) <- names(ordered_filter_list)

  for(i in 1:num_regis){

    # subset the parts that we care about
    li <- lapply(1:regi[[i]]$atlas$numRegions,
                 function(qq){
                   # hotfix because plate 102 has different length of regions and colors
                   # mind, only one square bracket!
                   no_color <- is.na(regi[[i]]$atlas$col[qq])

                     if (no_color) {

                     new_color <- "#cccccc"
                   } else {
                     new_color <- regi[[i]]$atlas$col[[qq]]
                   }

                   data.frame(
                     # right transformed
                     xrT = regi[[i]]$atlas$outlines[[qq]]$xrT,
                     yrT = regi[[i]]$atlas$outlines[[qq]]$yrT,
                     # left transformed
                     xlT = regi[[i]]$atlas$outlines[[qq]]$xlT,
                     ylT = regi[[i]]$atlas$outlines[[qq]]$ylT,
                     color = new_color,
                     # From wholebrain::plot.outlines
                     scale_factor = regi[[i]]$transformationgrid$width/dim(regi[[i]]$transformationgrid$mx)[2],
                     # TODO:
                     # check whether scale_factor can be extracted from regis[[i]]$resize
                     stringsAsFactors = FALSE)
                 })

    names(li) <- c(1:regi[[i]]$atlas$numRegions)

    output_list[[i]] <- bind_rows(li, .id = "contour.ID")



  }

  return(output_list)
}
