

#' @export
inspect_wrong_regi_plates <- function(setup){
  # TODO: if they have more than one format it will be a problem
  # check if they have been registered manually
  manual_regi <- list.files(setup$savepaths$out_registration, pattern = "tif", full.names = TRUE)

  # check auto_loop registration
  auto_regi <- list.files(setup$savepaths$out_auto_registration, '.tif',
                                  full.names = TRUE)


  # if no manual, go directly with the auto ones
  if(length(manual_regi) == 0){

    images_to_inspect <- auto_regi
  } else{
    # Put the manual first, auto will get counted as duplicated
    df <- dplyr::tibble(file_path = c(manual_regi, auto_regi),
                  base_file_path = basename(file_path)) %>%
      dplyr::mutate(flag = duplicated(base_file_path)) %>%
      filter(flag == FALSE)

    # get the images
    images_to_inspect <- df$file_path
  }



  li <- list()
  n_row <- length(images_to_inspect)

  for(i in 1:n_row){
    if(i==1){
      # first device
      quartz(width=20, xpos=200, ypos=200)
    }
    # load the image
    img <- imager::load.image(images_to_inspect[i])
    plot(img, axes=FALSE)

    correct <- readline(prompt="Was the match correct [enter] or incorrect [any other key]? :>")

    if(correct == ""){
      # save the value in li
      li[[i]] <- "correct"
    } else {
      li[[i]] <- "incorrect"
    }

  }
  if(any(li == "incorrect")){
    # should give us indices
    images_to_match <- which(li == "incorrect")
    # prompt the user
    message("You have indicated there are images that need correction.
            \nReturning AP plates to feed into regi_loop")

    plates <- stringr::str_extract(string = images_to_inspect[images_to_match],
                                   pattern="_plate_[0-9]+")
    plates <- stringr::str_extract(plates,
                                   pattern="[0-9]+")
    plates <- sort(as.numeric(plates))

    graphics.off()
    message("Done with checking!")
    return(plates)
  }

  return(message("GOOD LUCK!! You have no plates to change!"))


}
