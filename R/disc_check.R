#' Check discuss running environment
#'
#' Checks wether the working directory is set, what the options are, and wether the required executables are installed. If some are not available it gives platform specific tips on how to install them. The function has no arguments.
#'
#' @seealso \code{\link{disc_getwd}} to check the working directory, \code{\link{disc_setwd}} to set the working directory, \code{\link{disc_conf}} to read or set the options
#'
#' @export
disc_check <- function() {

  # get (and check) working directory
  wd <- disc_getwd(warn=TRUE)
  message("working directory : ", wd)

  # get, set and display options
  if ( file.exists(make_path(wd, "disc.conf")) ) {
    message("settings file exists. Settings :")
  } else  {
    message("settings file does not exist. Running with default settings :")
  }
  options <- disc_conf()
  print(options)

  # check executables

  check_exec("java", url="https://www.java.com/en/download/", notes="You need a java JRE")

  # check_exec("rsync")

  check_exec("exif")

  check_exec("convert", package="imagemagick")

  return(invisible(NULL))
}
