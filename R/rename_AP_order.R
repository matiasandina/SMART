#' @title User friendly way to put images into order and rename them before setup
#' @description Asks user for folder where images are stored.
#'   If image titles are not in AP order, it prompts user to establish order
#'   and rename files properly into Z planes
#' @param img_folder path of folder where images are stored
#' @param
#' @details

img_folder <- "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/small"

rename_AP_order <- function(img_folder, filetype=c(".tif")){

  files <- list.files(img_folder, pattern = filetype, full.names = TRUE)

  # helper for image equalization
  # see https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
  hist.eq <- function(im) imager::as.cimg(ecdf(im)(im), dim=dim(im))


  # enter for loop
  for (image in files){
    # show first image
    # TODO: fix the width issue
    window_title <- basename(image)
    quartz(width = 5, title=window_title)
    # img <- magick::image_read() # this way needs a lot of rescaling no?
    img <- imager::load.image(image)

    img %>% plot()

    # let's do a bit of enhancement of the channel
    quartz(width = 5, title = paste("Equalized", window_title))

    hist.eq(img) %>% plot()

    first_guess <- as.numeric(readline("what is your first guess for AP level? :> "))

    # generate options around the guess
    val_range <- c(0.1, 0.2, 0.3, 0.4)

    first_guess <- c(sort(first_guess - val_range),
                     first_guess,
                     sort(first_guess + val_range))
    # now sorting with decreasing = TRUE for AP correct order
    first_guess <- sort(first_guess, decreasing = TRUE)

    quartz(width=5)
    par(mfrow=c(3,3))
    # call first time and open a new device
    # pull_atlas(first_guess[1], new_device = TRUE)
    # call the rest with no new device
    purrr::map(first_guess[1:length(first_guess)], function(AP){
      pull_atlas(AP, new_device = FALSE, adj = c(-1, 1)) # center justified text
    })

    break("breaking for debug")

  }

}



