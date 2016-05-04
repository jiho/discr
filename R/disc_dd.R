#' Get or set discusr deployments directory
#'
#' This is the directory where deployments are stored (usually as a set of numbered folders). Data will be read from and written to this directory. Settings will also be stored there.
#'
#' @param deploy.dir path to the directory containing deployments (and associated options). By default "deployments" in the current directory.
#'
#' @details The standard behaviour is having a "deployments" directory in the current working directory of R. If this is not adequate, this function can set it to something else (an absolute path on another drive, another relative path such as "data/deployments", etc.). To make this choice persistent across sessions, set the option \code{disc.deploy.dir} in your \code{.Rprofile}.
#'
#' @return The path to the deployment directory, which has been checked for existence and writability.
#'
#' @export
# NB: most users should not have to care about this
disc_dd <- function(deploy.dir=NULL) {

  # first get it from the arguments
  dir <- deploy.dir

  # if not provided, get it from the options if set
  if ( is.null(dir) ) {
    dir <- getOption("disc.deploy.dir")

      # if not set in the options, set it to the default value
      if ( is.null(dir) ) {
        dir <- "deployments"
      }

  }

  # set it in the options for next time
  options(disc.deploy.dir=dir)

  # check the deployments directory
  if ( ! file.exists(dir) ) {
    stop("Deployments directory \"", dir, "\" does not exist")
  }
  if ( strtoi(file.info(dir)$mode) < 600 ) {
    warning("Deployments directory \"", dir, "\" is not writable")
  }

  return(dir)
}
