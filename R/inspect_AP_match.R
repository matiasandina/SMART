#' @title User friendly way to inspect whether `match_image_to_atlas` was good
#' @description Shows image and atlas to check whether tey match, let's user flag correct/incorrect.
#' @param match_df dataframe with image_paths and assigned APs (output of `match_image_to_atlas`)

#' @export
inspect_AP_match <- function(match_df) {

  li <- list()
  n_row <- nrow(match_df)

  for (i in 1:n_row) {
    if(i == 1){
      # first device
      quartz(width = 10)
      # second device
      quartz(width = 10)
    }
    # set current device to previous device
    dev.set(dev.prev())
    # pull atlas at specified AP
    pull_atlas(match_df$mm.from.bregma[i], new_device = FALSE)
    # load the image
    img <- imager::load.image(match_df$image_file[i])
    # set current device
    dev.set(dev.next())
    plot(img, axes = FALSE)

    correct <- readline(prompt = "Was the match correct [enter] or incorrect [any other key]? :>")

    if(correct == "") {
      # save the value in li
      li[[i]] <- "correct"
    } else {
      li[[i]] <- "incorrect"
    }

  }

  if (any(li == "incorrect")) {
    # should give us indices
    images_to_match <- which(li == "incorrect")
    # prompt the user
    message("You have indicated there are images that need correction.
            \nEntering match_image_to_atlas")
    fixed_images <- purrr::map(images_to_match,
                               function(image) match_image_to_atlas(ind_img = match_df$image_file[image]))

    fixed_images <- dplyr::bind_rows(fixed_images)

    # append to match_df
    # since we know the order we can use that to directly replace
    # we can check for duplicates later

    match_df[images_to_match, ] <- fixed_images

    # we can check dimensions for extra piece of mind
    if (n_row != nrow(match_df)) {
      stop("There has been a problem with the number of rows. Check code")
    }

    }

  graphics.off()
  message("Done with checking!")
  return(match_df)
}
