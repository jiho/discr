#' Project-level settings
#'
#' Read and write project-level settings
#'
#' @details
#' Settings are read, in order, from the defaults, then the project-level configuration file and finally from the arguments provided in \code{...}. Then those settings are set with the function \code{\link{options}} to be accessible from any other function in discuss. Finally, the non-default settings are stored in \code{file} in the project directory.
#'
#' @param ... named arguments with settings to set
#' @param file name of the configuration file, in the project directory
#' @param verbose when TRUE, options are shown after being read
#' @export
#' @importFrom plyr laply
disc_conf <- function(..., file="disc.conf", verbose=FALSE) {

  # set defaults
  defaults <- list(
    aquarium = "0,0,200,200",
    camera_angle = 90,
    diameter = 40,
    java_memory = 1024,
    storage_directory = ""
  )
  class(defaults) <- c("disc.settings", "list")

  # get settings from the project configuration file
  wd <- disc_getwd()
  file <- make_path(wd, file)
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

#' @keywords internal
print.disc.settings <- function(x) {
  n <- names(x)
  for (i in 1:length(x)) {
    cat("  ", n[i], " : ", x[[i]], "\n", sep="")
  }
  return(invisible(x))
}


#' Set discuss working directory
#'
#' The working directory is the directory where deployments are stored. Data will be read from and written to this directory.
#' Project level settings will also be stored there.
#'
#' @param dir character string giving the name of the directory to set as the working directory
#' @param persistent boolean; wether to store that working directory as default (storing \code{options(disc.wd=...)} in \code{.Rprofile})
#'
#' @return Returns the working directory but invisibly. Sets the working directory for the current session and for all future sessions if \code{persistent=TRUE}.
#' @export
disc_setwd <- function(dir, persistent=FALSE) {

  options(disc.wd=dir)

  if (persistent) {
    userProfile <- "~/.Rprofile"
   if (!file.exists(userProfile)) {
     file.create(userProfile)
   }
   cat("options(disc.wd=\"", dir, "\")", file=userProfile, append=TRUE)
   # TODO make it detect if the setting is already in .Rprofile and update it
  }

  return(invisible(dir))
}

#' @rdname disc_setwd
#' @export
dsetwd <- disc_setwd


#' Get discuss working directory
#'
#' @export
disc_getwd <- function() {
  # get it from the options
  wd <- getOption("disc.wd")

  # check it is usuable
  if ( is.null(wd) ) {
    stop("Working directory is not set. Set it with `disc_setwd`")
  }
  if ( ! file.exists(wd) ) {
    stop("Working directory ", wd, " does not exist")
  }
  if ( strtoi(file.info(wd)$mode) < 600 ) {
    stop("Working directory ", wd, " is now writable")
  }

  return(wd)
}

#' @rdname disc_getwd
#' @export
dgetwd <- disc_getwd
