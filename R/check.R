#' Check discuss running environment
#'
#' Check working directory, options, and presence of required executable
#'
#' @export
disc_check <- function() {

  # get (and check) working directory
  wd <- disc_getwd(warn=TRUE)
  message("working directory OK: ", wd)

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
  message("java executable found")

  # check_exec("rsync")
  # message("rsync executable found")

  check_exec("convert", package="imagemagick")
  message("imagemagick executables found")

  check_exec("exif")
  message("exif executable found")

  message("\nEverything looks right, you're all set.\n")
  return(invisible(NULL))
}
