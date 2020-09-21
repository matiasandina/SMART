#' @title Pull up atlas plate in graphics device.
#' @description Enter in an AP coordinate from the Allen Mouse Brain Atlas and
#'   pull up the atlas in a graphics device.
#' @param AP Anterior-posterior mouse atlas coordinate.
#' @param xpos x-position in pixels justified to the left of the main computer
#'   screen
#' @param x Coordinates where the text should be written. Argument from [text()]
#'   function.
#' @param adj one or two values in [0, 1] which specify the x (and optionally y)
#'   adjustment of the labels. On most devices values outside that interval will
#'   also work. Argument from [text()] function.
#' @param cex Character expension factor. Argument from [text()] function.
#' @param force_redraw boolean to force pull_atlas to redraw instad of getting the image
#' @export
pull_atlas <- function(AP, xpos = 0, x = 0, adj = c( -1, 0),
                       cex = 1.5, width=NULL, save=FALSE,
                       new_device=TRUE, force_redraw = FALSE){

  # hotfix -- I couldn't figure out the fun_call$AP problem
  # see here
  # https://stackoverflow.com/questions/63980673/using-match-call-in-recursive-function

  if(AP < -5.5){
    return("We cannot display AP < -5.5")
  }

  # catch the function call
  fun_call <- SMART:::match.call.defaults()

  window_title <- paste0("Plate ", toString(platereturn(AP)),
                         ", AP ", toString(round(roundAP(AP), digits=2)))
  # no width makes error o  n Ubuntu systems
  if(is.null(width) && get_os() == "linux"){
    width <- 10
  }

  # Manage window command ####
  # we can wrap this call in a purr::map with new_device=FALSE (plot more than one tile)
  if(new_device){
    if(get_os() == "osx"){
      quartz(width = width, title= window_title)
    } else {
      quartz(width = width,
             title = window_title, xpos = xpos)
    }
  }


  # try to find the png in the data first
  if(!force_redraw){
    AP_to_match <- round(roundAP(AP),2)
    AP_to_match <- paste0("AP_level_", AP_to_match,".png")
    path_to_try <- file.path(.libPaths()[1],"SMART","data", AP_to_match)

    # if exists, load it
    if(file.exists(path_to_try)){
      plot(imager::load.image(path_to_try), axes=FALSE)

    } else {
      print("File does not exist calling with force_redraw")
      # else, call the function again forcing redraw
      pull_atlas(AP = fun_call$AP, xpos=eval(fun_call$xpos),  fun_call$x,
                 fun_call$adj, fun_call$cex,
                 fun_call$width, fun_call$save,
                 fun_call$new_device, force_redraw = TRUE)
    }


  } else {
    print("enter else")
    # construct available options
    available_drawing_options <- tibble(
      plate.id = atlasIndex$plate.id,
      mm.from.bregma = atlasIndex$mm.from.bregma,
      plane = atlasIndex$plane
    ) %>%
      # TODO: Fix here if you want both planes
      filter(plane == "coronal") %>%
      mutate(plate = platereturn(mm.from.bregma),
             # ! NULL will give TRUE if exists
             coronal_plate_exists = !sapply(EPSatlas$plate.info, is.null))
    print("enter tryCatch")

    tryCatch({
      # try re-draw
      wholebrain::schematic.plot(dataset = NULL, mm.grid=F, coordinate = roundAP(AP), region.colors =  TRUE, device = F)
      # this eval is just in case the adj gets passed as c(...)
      text(x = x, window_title, adj = eval(adj), cex = cex)

    }, error = function(e) {
      message("These are the AP levels we can draw:")
      message("----------------------------")
      print(
      available_drawing_options %>%
        filter(coronal_plate_exists == TRUE) %>%
        pull(mm.from.bregma)
      )
      message("----------------------------")
      message(paste("We can't draw the AP level you gave (AP =", fun_call$AP, ")"))
    })

    # save plot here ? could be useful to people just wanting to get atlas plates ?
    # }

  }

}


