#' Get, set of check discuss deployments directory
#'
#' This is the directory where deployments are stored (usually as a set of numbered folders). Data will be read from and written to this directory. Settings will also be stored there.
#'
#' @details If the standard behaviour of having a "deployments" directory in the current working directory is not satisfactory, \code{setdd} can be used to set it to something else (an absolute path on another drive, another relative path such as "data/deployments", etc.). To make this choice persistent across sessions, set the option \code{disc.deploy.dir} in your \code{.Rprofile}.
#'
#' @return \code{getdd} returns the deployments directory; \code{checkdd} checks if it exists and is writable and returns it invisibly; \code{setdd} sets the deployments directory for the current session and returns it invisibly.
#'
#' @export
#' @keywords internal
# NB: most users should not have to care about this, so we make the functions internal
#     basically the convention is to have a "deployments" directory as a sub-directory of your project and that's it
getdd <- function() {

  # get it from the options if set
  dir <- getOption("disc.deploy.dir")

  # when it is not set, set it (to the default)
  if ( is.null(dir) ) {
    dir <- setdd()
  }

  return(wd)
}

#' @param dir character string giving the path to the directory to set as the working directory; either relative or absolute. By default: "deployments"
#'
#' @export
#' @keywords internal
#' @rdname getdd
checkdd <- function(dir) {
  # check the deployments directory
  if ( ! file.exists(dir) ) {
    stop("Deployments directory \"", dir, "\" does not exist")
  }
  if ( strtoi(file.info(dir)$mode) < 600 ) {
    warning("Deployments directory \"", dir, "\" is not writable")
  }
  return(invisible(dir))
}

#' @export
#' @keywords internal
#' @rdname getdd
setdd <- function(dir="deployments") {

  options(disc.deploy.dir=dir)

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
