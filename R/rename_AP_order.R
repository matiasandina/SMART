#' @title User friendly way to put images into order and rename them before setup
#' @description Asks user for folder where images are stored.
#'   If image titles are not in AP order, it prompts user to establish order
#'   and rename files properly into Z planes
#' @param img_folder path of folder where images are stored
#' @param filetype type of the files, pattern fed to `list.files(...)`
#' @details

# TODO: Change name of the function to something more proper

# img_folder <- "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/small"
#' @export
rename_AP_order <- function(img_folder, filetype=c(".tif")){

  # load atlasIndex
  if(!exists("atlasIndex", envir = .GlobalEnv)){
  load(file.path(.libPaths()[1],"wholebrain","data","atlasIndex.RData"))
  }

  files <- list.files(img_folder, pattern = filetype, full.names = TRUE)

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
    quartz(width = 5, title=window_title)
    # img <- magick::image_read() # this way needs a lot of rescaling no?
    img <- imager::load.image(image)

    plot(img)

    # let's do a bit of enhancement of the channel
    quartz(width = 5, title = paste("Equalized", window_title))

    # equalized histogram
    plot(hist.eq(img))

    first_guess <- as.numeric(readline("what is your first guess for AP level? :> "))

    # generate options around the guess (overwrites guess)
    first_guess <- generate_AP_options(first_guess, resolution=0.2)

    quartz(width=5)
    par(mfrow=c(3,3))
    # call first time and open a new device
    # pull_atlas(first_guess[1], new_device = TRUE)
    # call the rest with no new device
    purrr::map(first_guess, function(AP){
      pull_atlas(AP, new_device = FALSE, adj = c(-1, 1)) # center justified text
    })


    done <- FALSE
    while(!done){
      message("+++++++++++++++++++++++++")
      message("Please see your options below:")
      message("+++++++++++++++++++++++++")

      my_options <- roundAP(first_guess)
      message(paste0("Option ", 1:length(my_options), ") ", round(my_options, 2), "\n"))

      guess_again <- readline("Use numbers to select the option that best fits your image :> ")
      guess_again <- as.numeric(guess_again)

      if(guess_again < 1 | guess_again > length(my_options)){
        stop("Error: option is not correct. Start again")
      }

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

      vec <-rep(0, 9)
      vec[selected_panel] <- 1

      # split into rows of 3
      spli <- split(vec, ceiling(seq_along(vec)/3))

      # find the rows and cols
      row <- which(sapply(spli, sum) == 1)
      column <- which(unlist(spli[row]) == 1)

      # Display red box for selection :D
      par(mfg = c(nr = row, nc = column))
      box(lty = '1373', col = 'red', lwd=6)

      done_str <- readline("Are you done? (Y/N) :> ")

      # get the mfg to the end
      par(mfg = c(nr = 3, nc =3))
      # plot empty plot
      # This is to prevent overplotting on the next iteration
      plot(0,type='n',axes=FALSE,ann=FALSE)


      if(tolower(done_str) == "y"){
        # assign to df the new value
        df[which(df$image_file == image), "mm.from.bregma"] <- second_guess
        # Do the saving accordingly
        enter_saving <- TRUE
        while(enter_saving){
          save_progress <- readline("Do you want to save (Y/N)? :> ")
            if(tolower(save_progress)=="y"){

              saveRDS(df, file=file.path(dirname(img_folder), "atlas_img_path_df"))
              message("Progress was saved, moving on :)")
              enter_saving <- FALSE
          }
            else if(tolower(save_progress)=="n"){
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

  return(df)
  }






