#' Check discusr running environment
#'
#' Checks the deployments directory (including wether a non-standard one is set), the options set in this deployments directory, and wether all required executables are installed. If some are not available it gives platform specific tips on how to install them. The function has no arguments.
#'
#' @inheritParams disc_dd
#'
#' @seealso \code{\link{disc_dd}} to get or set the deployments directory, \code{\link{disc_conf}} to read or set options
#'
#' @export
disc_check <- function() {

  # get (and check) working directory
  wd <- disc_dd()
  message("deployments directory : ", wd)

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
