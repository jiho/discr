#' Get or set discuss deployments directory
#'
#' This is the directory where deployments are stored (usually as a set of numbered folders). Data will be read from and written to this directory. Settings will also be stored there.
#' 
#' @details If the standard behaviour of having a "deployments" directory in the current working directory is not satisfactory, \code{setdd} can be used to set it to something else (an absolute path on another drive, another relative path such as "data/deployments", etc.). To make this choice persistent across sessions, set the option \code{disc.deployments} in your \code{.Rprofile}.
#'
#' @return \code{getdd} returns the deployments directory; \code{setdd} and sets the deployments directory for the current session and returns it invisibly
#'
#' @export
#' @keywords internal
# NB: most users should not have to care about this, so we make the functions internal
#     basically the convention is to have a "deployments" directory as a sub-directory of your project and that's it
getdd <- function(warn=FALSE) {

  # get it from the options if set
  wd <- getOption("disc.deployments")

  # when it is not set, set it (to the default)
  if ( is.null(wd) ) {
    wd <- setdd()
  }

  # check wether it is usable
  if ( ! file.exists(wd) ) {
    stop("Working directory \"", wd, "\" does not exist")
  }
  if ( strtoi(file.info(wd)$mode) < 600 ) {
    stop("Working directory \"", wd, "\" is not writable")
  }

  return(wd)
}

#' @param dir character string giving the path to the directory to set as the working directory; either relative or absolute. By default: "deployments"
#'
#' @export
#' @keywords internal
#' @rdname getdd
setdd <- function(dir="deployments") {

  options(disc.deployments=dir)

  # TODO explore this possibility further
  # if (persistent) {
  #   userProfile <- "~/.Rprofile"
  #     if (!file.exists(userProfile)) {
  #     file.create(userProfile)
  #   }
  #   cat("options(disc.wd=\"", dir, "\")\n", sep="", file=userProfile, append=TRUE)
  #   # TODO make it detect if the setting is already in .Rprofile and update it
  # }

  return(invisible(dir))
}

