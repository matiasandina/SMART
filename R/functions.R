#' @title Helper background functions
#' @description  necessary functions and data structures to load
#' @family aggregate functions
# # Load whole brain atlas index
# load(file.path(.libPaths()[1],"wholebrain","data","atlasIndex.RData"))

# Load whole brain atlasIndex values and extract only coronal slices
# at_ind  <- atlasIndex$mm.from.bregma[1:132]

#' @export
# return plate number based on entered AP
platereturn <- function(AP) {

  # we can switch to "sagittal" if needed
  atlas_sub <- atlasIndex[atlasIndex$plane == "coronal", "mm.from.bregma"]
  ind <- sapply(AP, function(x) which.min(abs(x - atlas_sub)))

  return(ind)
}

#' @export
# Get Allen Mouse Brain Atlas Return
AMBA_return <- function(AP){
  ind <- platereturn(AP)
  return(atlasIndex$plate.id[ind])
}

#' @export
# Rounds to nearest atlas coordinate in whole brain (coronal)
roundAP <- function(AP, plane="coronal") {
  # check input
  if(!plane %in% c("coronal", "sagittal")){
    stop(sprintf("Plane must be `coronal`` or `sagittal`.\n`%s` was provided", plane))
  }

  # perform atlasIndex subset only once outside the loop
  atlas_sub <- atlasIndex[atlasIndex$plane == plane, "mm.from.bregma"]
  # get the indices of the closest positions to AP queries
  ind    <- sapply(AP, function(x) which.min(abs(x - atlas_sub)))
  # subset the mm from bregma
  vec <- atlas_sub[ind]
  return(vec)
}

#' @export
# Get cell counts of data based on regions of interest given
get_count <- function(dataset, roi = c('MO', 'TH')){
  out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x))))
  roi.data <- data.frame(acronym = roi, cell.count = out)
  return(roi.data)
}

#' @export
# Recursive function to get all child regions of entered regions of interest
get_all_children <- function (rois, children = c()) {
  for (l in 1:length(rois)){
    # Get child regions
    new_children <- wholebrain::get.acronym.child(rois[l])

    # If there are more child regions
    if (!anyNA(new_children)) {
      children <- c(children, new_children)
      children <- get_all_children(new_children, children = children)

    }
  }
  return(children)
}

#' @export
# Get current OS
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

#' @export
# convertpath for windows users
convertpath <- function(path){
  return(gsub("\\\\", "/", path, ignore.case = FALSE, perl = FALSE,
       fixed = FALSE, useBytes = FALSE))
}

#' @export
# Get counts based on hemisphere
get_hemi_cnts <- function(dataset, roi = c('MO', 'TH'), hemi= "Right"){
  if (hemi == "Right") {
    out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x) * dataset$right.hemisphere)))
  } else if (hemi == "Left") {
    out <- unlist(lapply(roi, function(x)sum(dataset$acronym %in% wholebrain::get.sub.structure(x) * !dataset$right.hemisphere)))
  }
  roi.data <- data.frame(acronym = roi, cell.count = out)
  return(roi.data)
}

#' @export
# This function allows to generate options in the vecinity of selected AP
# It sorts them from most anterior (positive) to most posterior (negative)
generate_AP_options <- function(AP, resolution){

  # let's make values 4 values to each side from given resolution
  val_range <- 1:4 * resolution

  values <- c(sort(AP - val_range),
                   AP,
                   sort(AP + val_range))
  # now sorting with decreasing = TRUE for AP correct order
  values <- sort(values, decreasing = TRUE)

  return(values)
}

#' This functions allow the user to choose a directory interactively
#' it is supposed to be cross-platform
#' comes from https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r

# First a helper function to load packages, installing them first if necessary
# Returns logical value for whether successful
#' @export
ensure_library = function (lib.name){
  x = lib.name %in% installed.packages()
  #if (!x) {
  #  install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
  #  x = require(lib.name, quietly = TRUE, character.only = TRUE)
  #}
  return(x)
}


#' @export
select_directory_method = function() {
  # Tries out a sequence of potential methods for selecting a directory to find one that works
  # The fallback default method if nothing else works is to get user input from the console
  if (!exists('.dir.method')){  # if we already established the best method, just use that
    # otherwise lets try out some options to find the best one that works here
    if (exists('utils::choose.dir')) {
      .dir.method = 'choose.dir'
    } else if (rstudioapi::isAvailable() & rstudioapi::getVersion() > '1.1.287') {
      .dir.method = 'RStudioAPI'
      ensure_library('rstudioapi')
    } else if(ensure_library('tcltk') &
              class(try({tt  <- tktoplevel(); tkdestroy(tt)}, silent = TRUE)) != "try-error") {
      .dir.method = 'tcltk'
    } else if (ensure_library('gWidgets2') & ensure_library('RGtk2')) {
      .dir.method = 'gWidgets2RGtk2'
    } else if (ensure_library('rJava') & ensure_library('rChoiceDialogs')) {
      .dir.method = 'rChoiceDialogs'
    } else {
      .dir.method = 'console'
    }
    assign('.dir.method', .dir.method, envir = .GlobalEnv) # remember the chosen method for later
  }
  return(.dir.method)
}



#' @export
choose_directory <- function(ini_dir = getwd(),
                             method = select_directory_method(),
                             title = 'Select data directory') {

  switch(method,
         'choose.dir' = choose.dir(default = ini_dir, caption = title),
         'RStudioAPI' = rstudioapi::selectDirectory(path = ini_dir, caption = title),
         'tcltk' = tcltk::tk_choose.dir(default = ini_dir, caption = title),
         'rChoiceDialogs' = rChoiceDialogs::rchoose.dir(default = ini_dir, caption = title),
         'gWidgets2RGtk2' = gfile(type = 'selectdir', text = title, initial.dir = ini_dir),
         readline('Please enter directory path: ')
  )
}

# add a function to get function arguments
# see https://stackoverflow.com/questions/14397364/match-call-with-default-arguments
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}
