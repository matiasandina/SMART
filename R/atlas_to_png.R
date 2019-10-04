# drawing the atlas each time is computationally expensive
# this thing will save into a the data directory for the package
# the script is wrapped in a function so that it doesn't run when sourced

atlas_to_png <- function(){
  # load atlas
  load(file.path(.libPaths()[1], "wholebrain", "data", "atlasIndex.RData"))


  sections <- dplyr::filter(atlasIndex, plane == "coronal")
  sections <- dplyr::pull(sections, mm.from.bregma)

  # sections from -5.58 cannot be shown now
  # 5.14 and 5.34 are also not available
  sections <- sections[sections > -5.5 & sections != 5.14 & sections != 5.34]

  # for some reason some sections are not available so we tryCatch
  X11()
  for(i in sections){
    print(i)
    ind_file <- paste0("AP_level_", round(i, 2), ".png")
    savepath <- file.path(getwd(), "data", ind_file)

    tryCatch(
      expr = {
        Cairo::CairoPNG(filename = savepath)
        pull_atlas(AP = i, new_device = FALSE, force_redraw = TRUE)
        # give some time
        Sys.sleep(2)
        graphics.off()
        X11()
      },
      error = function(e){
        # (Optional)
        # Do this if an error is caught...
        message("AP not found")
        Sys.sleep(2)
        graphics.off()
        X11()

      },
      warning = function(w){
        # (Optional)

      },
      finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
      }
    )


  }



}
