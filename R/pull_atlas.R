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
#' @export
pull_atlas <- function(AP, xpos = 0, x = 0, adj = c( -1, 0),
                       cex = 1.5, width=NULL, save=FALSE,
                       new_device=TRUE){
  # no width makes error on Ubuntu systems
  if(is.null(width) && get_os() == "linux"){
    width <- 5
  }
  window_title <- paste0("Plate ", toString(platereturn(AP)),", AP ", toString(round(roundAP(AP), digits=2)))
  # we can wrap this call in a purr::map with new_device=FALSE (plot more than one tile)
  if(new_device){
    quartz(width = width,
           title = window_title, xpos = xpos)

  }
  wholebrain::schematic.plot(dataset = NULL, mm.grid=F, coordinate = roundAP(AP), region.colors =  TRUE, device = F)
  text(x = x, window_title, adj = adj, cex = cex)

  # if(save){
  # save plot here ? could be useful to people just wanting to get atlas plates ?
  # }

}


