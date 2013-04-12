#
#      Utility functions
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------

make_path <- function(...) {
  #
  # Create a valid path from one or several path elements
  #

  # get all arguments
  args <- list(...)

  # paste them together
  nArgs <- length(args)
  path <- args[[1]]
  if (nArgs > 1) {
    for (i in 2:nArgs) {
      path <- paste(path, args[[i]], sep="/")
    }
  }

  # make sure the path is valid and clean (perform path extension, remove repeated path separators, etc.)
  path <- normalizePath(path, winslash="/", mustWork=FALSE)

  return(path)
}

check_status <- function(status, message="discus error", ...) {
  #
  # Check the exit status of a command (typically run through system())
  #
  # status  status code, usually an integer
  # message error message
  # ...     passed to stop
  #

  if ( status !=0 ) {
    stop(message)
  }

  return(invisible(status))
}

find_os <- function() {
  #
  # Try to detect various common OSes
  #

  suppressPackageStartupMessages(require("stringr", quietly=TRUE))

  if ( .Platform$OS.type == "unix" ) {
    # find more info about the OS from the command line
    os <- tolower(system("uname -a", intern=TRUE, ignore.stderr=TRUE))

    if ( str_detect(os, "darwin") ) {
      os <- "mac"

    # try to discriminate through a variety of linux OSes
    } else if ( any(str_detect(os, c("ubuntu", "debian"))) ) {
      os <- "debian-like"
    } else if ( any(str_detect(os, c("red", "rhel", "fedora"))) ) {
      os <- "redhat-like"
    } else if ( any(str_detect(os, c("suse", "slse"))) )
      os <- "suse-like"
    else {
      os <- "other unix"
    }

  } else {
    os <- "windows"
  }

  return(os)
}

check_software <- function(exec, url="", package=exec, notes="", ...) {
  #
  # Check that a given executable can be found in the path and return its full path
  # If it cannot be found, provide some installation instructions
  #
  # exec    name of the exectuable
  # url     link to point the user to when the executable is absent
  # package name of the package usually providing this exectuable, used to
  #         indicate how to install the software when url is not provided
  # notes   further notes appended to the error message
  # ...     passed to stop()
  #

  suppressPackageStartupMessages(require("stringr", quietly=TRUE))

  # find where the executable is installed
  exePath <- Sys.which(exec)

  # if if cannot be found
  if (exePath == "") {
    if (url != "") {
      # if a url is provided, point to it
      install <- str_c("See ", url, " for instructions")
    } else {
      # otherwise, try to provide some OS-specific installation instructions
      os <- find_os()
      if (os == "mac") {
        install <- str_c("Install homebrew: http://mxcl.github.io/homebrew/\n",
                         "Then, install ", package, " with: brew install ", package)
      } else if (os == "debian-like") {
        install <- str_c("Install it with: sudo apt-get install ", package)
      } else if (os == "redhat-like") {
        install <- str_c("Install it with: sudo yum install ", package)
      } else if (os == "suse-like") {
        install <- str_c("Install it with: sudo zypper install ", package)
      } else if (os == "windows") {
        stop("Windows is not supported at this time")
      } else {
        install <- str_c("Search your package manager for ", package)
      }
    }

    # in all cases, error-out while giving a (hoprefully) informative message
    stop(exec, " is missing\n", install, "\n", notes, ...)
  }

  # return the full path to the executable
  return(exePath)
}
