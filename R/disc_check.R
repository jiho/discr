#' Check discuss running environment
#'
#' Check working directory, options, and presence of required executable
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
