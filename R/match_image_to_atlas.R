#' @title User friendly way to put images into order with atlas
#' @description Asks user for folder where images are stored.
#'   It prompts user to align images to better suited plate on atlas.
#' @param img_folder path of folder where images are stored
#' @param ind_img individual image in case this function needs to be called on one image.
#'  Save not possible in this case.
#' @param filetype type of the files, pattern fed to `list.files(...)`

#' @export
match_image_to_atlas <- function(img_folder=NULL, ind_img=NULL,
                                 filetype=c(".tif")) {

  # load atlasIndex
  if (!exists("atlasIndex", envir = .GlobalEnv)) {
  load(file.path(.libPaths()[1], "wholebrain", "data", "atlasIndex.RData"))
  }

  # throw error if both the img_folder and ind_img are null
  if(is.null(img_folder) & is.null(ind_img)){
    stop("Both `img_folder` & `ind_img` are NULL. Please provide one non NULL argument")
  }

  if(is.null(ind_img)){
    files <- list.files(img_folder, pattern = filetype, full.names = TRUE)
  } else {
    # might have path problems here..
    files <- ind_img
  }

  # helper for image equalization
  # see https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
  hist.eq <- function(im) imager::as.cimg(ecdf(im)(im), dim=dim(im))

  # Make space for image coordinates
  df <- data.frame(image_file = files,
                   mm.from.bregma = NA,
                   stringsAsFactors = FALSE)

  # calculate this to provide some feedback
  images_to_go <- length(files)

  # enter for loop
  for (image in files){
    # show first image
    # TODO: fix the width issue
    window_title <- basename(image)
    quartz(width = 10, title=window_title)
    # img <- magick::image_read() # this way needs a lot of rescaling no?
    img <- imager::load.image(image)

    plot(img, axes=FALSE)

    # let's do a bit of enhancement of the channel
    quartz(width = 10, title = paste("Equalized", window_title))

    # equalized histogram
    plot(hist.eq(img), axes = FALSE)

    first_guess <- readline("what is your first guess for AP level? :> ")

    while (is.na(as.numeric(first_guess))) {
      message("Your input is not valid, please enter a number.")
      guess_again <- readline("what is your first guess for AP level? :> ")
    }

    first_guess <- as.numeric(first_guess)

    # generate options around the guess (overwrites guess)
    first_guess <- generate_AP_options(first_guess, resolution = 0.2)

    quartz(width = 5)
    par(mfrow = c(3, 3),
            oma = c(1, 1, 0, 0) + 0.1,
            mar = c(0, 0, 0, 0) + 0.1)
    # call first time and open a new device
    # pull_atlas(first_guess[1], new_device = TRUE)
    # call the rest with no new device
    purrr::map(first_guess, function(AP){
      pull_atlas(AP, new_device = FALSE, adj = c(-1, 1)) # center justified text
    })


    done <- FALSE
    while (!done) {
      message("+++++++++++++++++++++++++")
      message("Please see your options below:")
      message("+++++++++++++++++++++++++")

      my_options <- roundAP(first_guess)
      message(paste0("Option ", seq_len(length(my_options)), ") ", round(my_options, 2), "\n"))

      guess_again <- readline("Use numbers to select the option that best fits your image :> ")
      # We check whether as.numeric will return NA
      # if we don't do this correctly, it will crash the program
      while (is.na(as.numeric(guess_again))) {
        message("Your input is not valid, please enter a number.")
        guess_again <- readline("Use numbers to select the option that best fits your image :> ")
      }

      # Now we convert for real
      guess_again <- as.numeric(guess_again)

      # if your numeric option is less than 1 or greater than the list
      # ask again
      while (guess_again < 1 | guess_again > length(my_options)) {
        message("Option is not valid. Choose again")
        guess_again <- readline("Use numbers to select the option that best fits your image :> ")
        while (is.na(as.numeric(guess_again))) {
          message("Option is not valid. Choose again")
          guess_again <- readline("Use numbers to select the option that best fits your image :> ")
          # Convert to number
          guess_again <- as.numeric(guess_again)
        }
      }
      # Subset the options on that number
      second_guess <- my_options[guess_again]
      # overwrite first guess & generate new values
      first_guess <- generate_AP_options(second_guess, 0.1)

      # find the selected panel in the new options
      selected_panel <- which(first_guess == my_options[guess_again])

      message(sprintf("You have chosen the option %s (AP = %s)",
                      as.character(guess_again),
                      as.character(second_guess)))
      message("Check the plates again!")

      purrr::map(first_guess, function(AP){
        pull_atlas(AP, new_device = FALSE, adj = c(-1, 1)) # center justified text
      })

      # calculate the position of the red box
      # if things work as expected, it should always be position 5
      # c(2,2) on the grid

      vec <- rep(0, 9)
      vec[selected_panel] <- 1

      # split into rows of 3
      spli <- split(vec, ceiling(seq_along(vec)/3))

      # find the rows and cols
      row <- which(sapply(spli, sum) == 1)
      column <- which(unlist(spli[row]) == 1)

      # Display red box for selection :D
      par(mfg = c(nr = row, nc = column))
      box(lty = "1373", col = "red", lwd = 6)

      done_str <- readline("Are you done? (Y/N) :> ")

      # get the mfg to the end
      par(mfg = c(nr = 3, nc = 3))
      # plot empty plot
      # This is to prevent overplotting on the next iteration
      plot(0, type = 'n', axes = FALSE, ann = FALSE)


      if (tolower(done_str) == "y") {
        # assign to df the new value
        df[which(df$image_file == image), "mm.from.bregma"] <- second_guess
        # Do the saving accordingly
        enter_saving <- TRUE
        while (enter_saving) {
          save_progress <- readline("Do you want to save (Y/N)? :> ")
            if (tolower(save_progress) == "y") {
              if (is.null(ind_img)) {
                saveRDS(df, file = file.path(dirname(img_folder),
                                             "atlas_img_path_df"))
                message("Progress was saved, moving on :)")
                enter_saving <- FALSE
              } else {
                 message("Cannot save individual image, use the return object")
                 enter_saving <- FALSE
                }
          }
            else if (tolower(save_progress) == "n") {
            message("Progress was not saved!")
            enter_saving <- FALSE
          }
            else{
            message("option not valid")
          }

          }


        done <- TRUE
      }
    }

    # Close all graphs
    graphics.off()
    cat(sprintf("Done with %s of %s images.",
                as.character(which(files == files[image])),
                as.character(images_to_go)))
    cat("Moving to next image")
  }

  # return par
  par(mfrow = c(1, 1))
  return(df)
  }
