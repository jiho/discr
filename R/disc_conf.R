#' Project-level settings
#'
#' Read and write project-level settings
#'
#' @details
#' Settings are read, in order, from the defaults, then the configuration file in the deployments directory and finally from the arguments provided in \code{...}. Then those settings are set with the function \code{\link[base]{options}} to be accessible from any other function in \code{discr}. Finally, the non-default settings are stored in the file \code{"disc.conf"} in the deployments directory.
#'
#' @param ... named arguments with settings to set
#' @inheritParams disc_dd
#' @param verbose when TRUE, options are shown after being read
#' @export
#' @importFrom plyr laply
#'
#' @examples
#' disc_conf()
#' # increase java memory to 2G
#' disc_conf(java_memory=2048)
#' # change diameter of arena to 40 cm (large DISC)
#' disc_conf(diameter=40)
disc_conf <- function(..., deploy.dir=NULL, verbose=FALSE) {

  # set defaults
  defaults <- list(
    aquarium = "0,0,200,200",
    diameter = 20,
    java_memory = 1024
  )
  # TODO set a default verbosity flag
  class(defaults) <- c("disc_conf", "list")

  # get settings from the project configuration file
  wd <- disc_dd(deploy.dir)
  file <- make_path(wd, "disc.conf")
  if (file.exists(file)) {
    inFile <- dget(file)
  } else {
    # if it does not exist, just use the defaults
    inFile <- defaults
  }

  # get provided arguments
  inArgs <- list(...)
  # allow expansion
  # NB: that also checks that option names are valid (i.e. in defaults)
  names(inArgs) <- laply(names(inArgs), match.arg, choices=names(defaults))

  # create the new settings
  # start with the defaults, add the settings currently set in the conf file, and finally add the settings provided on the command line
  settings <- defaults
  settings[match(names(inFile), names(settings))] <- inFile
  # TODO here something wrong can happen if a setting exists in the conf file but not in the defaults
  settings[match(names(inArgs), names(settings))] <- inArgs

  # set those options in R
  rSettings <- settings
  names(rSettings) <- paste("disc.", names(settings), sep="")
  options(rSettings)

  # display them if requested
  if (verbose) {
    message("Current settings")
    print(settings)
  }

  # extract the non default settings
  settingsNames <- names(settings)
  different <- laply(settingsNames, function(x) {
    settings[[x]] != defaults[[x]]
  })
  newSettings <- settings[different]
  if (length(newSettings) != 0) {
    # write them to the config file
    dput(newSettings, file=file)
  }

  return(settings)
}

#' @rdname disc_conf
#' @export
dconf <- disc_conf

#' @rdname disc_conf
#' @export
#' @keywords internal
print.disc_conf <- function(x, ...) {
  n <- names(x)
  for (i in 1:length(x)) {
    cat("  ", n[i], " : ", x[[i]], "\n", sep="")
  }
  return(invisible(x))
}
