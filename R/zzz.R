#' @title On attachment of SMART
#' @description setup the quartz function depending on the user platform

.onAttach<-function(...){
  # Initiate quartz
  if(get_os() == "windows") {
    quartz<-function(width, height, ...){windows(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  } else if(get_os() == "linux") {
    quartz<-function(width, height, ...){x11(width, height, ...)}
    assign("quartz", quartz, envir = .GlobalEnv)
  } else{
    # macOS quartz does not allow xpos
    # we can at least capture xpos and toss it
    quartz <- function (title, width, height, pointsize, family, antialias,
                        type, file = NULL, bg, canvas, dpi, xpos=NULL, ypos=NULL)
    {
      if (missing(type) || type %in% c("", "native", "Cocoa")) {
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop"))
          stop(msg, domain = NA)
        else if (identical(check, "warn"))
          warning(msg, immediate. = TRUE, noBreaks. = TRUE,
                  domain = NA)
      }

      if(!is.null(xpos)){
        warning("MacOS quartz does not accept `xpos`. Not using parameter")
      }
      if(!is.null(xpos)){
        warning("MacOS quartz does not accept `ypos`. Not using parameter")
      }

      new <- list()
      if (!missing(title))
        new$title <- title
      if (!missing(width))
        new$width <- width
      if (!missing(height))
        new$height <- height
      if (!missing(pointsize))
        new$pointsize <- pointsize
      if (!missing(family))
        new$family <- family
      if (!missing(antialias))
        new$antialias <- antialias
      if (!missing(bg))
        new$bg <- bg
      if (!missing(canvas))
        new$canvas <- canvas
      if (!missing(type))
        new$type <- type
      if (!missing(dpi))
        new$dpi <- dpi
      if (!grDevices:::checkIntFormat(new$title))
        stop("invalid 'title'")
      if (!is.null(file) && !grDevices:::checkIntFormat(file))
        stop("invalid 'file'")
      d <- check.options(new, name.opt = ".quartz.Options", envir = .Quartzenv)
      .External(C_Quartz, d$type, file, d$width, d$height, d$pointsize,
                d$family, d$antialias, d$title, d$bg, d$canvas, if (is.na(d$dpi)) NULL else d$dpi)
      invisible()
    }
    assign("quartz", quartz, envir = .GlobalEnv)
  }
}
