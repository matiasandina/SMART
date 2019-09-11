#' @title User friendly way to look for conflicts of alignment and rename images with Z planes
#' @description Receives df from previous pipeline step, looks for conflicts and renames the images adding Z numbers
#' @param match_df df that comes from `match_image_to_atlas` function
#' @import dplyr
#' @export
rename_AP <- function(match_df){

  # helper for image equalization
  # see https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
  hist.eq <- function(im) imager::as.cimg(ecdf(im)(im), dim=dim(im))

  # look for conflict
  conflict <- janitor::get_dupes(match_df, mm.from.bregma)

  if(nrow(conflict) > 0){
    message("We have found n>1 images assigned to same AP level")

    new_coordinates <- data.frame(NULL, stringsAsFactors = FALSE)

    # in case there's more than one conflict we will need this for loop
    for(AP in unique(conflict$mm.from.bregma)){
      # subset the individual conflict
      query <- conflict[conflict$mm.from.bregma == AP, ]
      # Show the atlas at that point
      pull_atlas(AP)
      img_counter <- 1
      for(image in query$image_file){
        window_title <- basename(image)
        img <- imager::load.image(image)
        # let's do a bit of enhancement of the channel
        quartz(width = 10, title = paste("Equalized", window_title))
        # equalized histogram
        plot(hist.eq(img), main = paste0(img_counter, ") ", basename(image)), axes=FALSE)
        img_counter <- img_counter + 1
      }

      # Decide which one goes first
      message("Options:")
      message(paste0(1:length(query$image_file), ')', basename(query$image_file), '\n'))

      # Ask which one fits better
      which_better <- readline("Which image corresponds better to atlas [1-n]? :> ")
      which_better <- as.numeric(which_better)

      message("Trying to find new AP positions for another image(s)")

      # remove that row from query
      # call
      img_to_fix <- query[-which_better, "image_file"]

      new_planes <- purrr::map(img_to_fix, function(image) match_image_to_atlas(ind_img=image))

      # bind rows to the new ones
      new_coordinates <- dplyr::bind_rows(new_coordinates, new_planes)

    }

    # create flags to mark the old/new time
    match_df$time_point <- 0
    new_coordinates$time_point <- 1


    # TODO: it could be that a new assigned coodinate is the same as an old one
    # in this case we would need to approximate between old and new (we use the mean)
    # check whether this is correct or not...
    original_val <- unique(match_df$mm.from.bregma)

    match_df <- match_df %>%
      bind_rows(new_coordinates) %>%
      # create a flag to see whether the new AP values were in the original AP values
      mutate(flag = ifelse(time_point == 1 & mm.from.bregma %in% original_val, TRUE, FALSE)) %>%
      # group by image to average
      group_by(image_file) %>%
      # calculate mean AP if flag
      mutate(new_AP = ifelse(flag, mean(mm.from.bregma), mm.from.bregma))%>%
      # select the important stuff
      dplyr::select(image_file, new_AP, time_point) %>%
      # filter
      filter(time_point == last(time_point)) %>%
      # rename new = old
      rename(mm.from.bregma = new_AP)%>%
      ungroup() %>%
      dplyr::select(-time_point)


  }

  message("\nAll images have unique AP. Renaming with Z planes")

  z_values <- stringr::str_pad(1:nrow(match_df), width=3, side="left", pad=0)
  z_values <- paste0("Z", z_values)

  match_df <- match_df %>%
    # arrange!
    arrange(desc(mm.from.bregma))%>%
    mutate(new_names = paste0(tools::file_path_sans_ext(image_file),
                              "_",
                              z_values,
                              ".tif")
           )

  # DO THE RENAMING HERE
  file.rename(from = match_df$image_file, to=match_df$new_names)

  match_df <- dplyr::select(match_df, new_names, mm.from.bregma) %>%
    rename(image_file = new_names)

  return(match_df)


}

