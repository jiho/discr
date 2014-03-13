#' Guess the operating system
#'
#' @keywords internal
#' @importFrom stringr str_detect
guess_os <- function() {

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


#' Check the existence of an executable
#'
#' Check that a given executable can be found in the path and return its full path. If it cannot be found, provide some installation instructions
#'
#' @param exec name of the exectuable
#' @param url link to point the user to when the executable is absent
#' @param package name of the package usually providing this exectuable, used to indicate how to install the software when url is not provided
#' @param notes further notes appended to the error message
#' @aram ... passed to stop()
#'
#' @keywords internal
#' @importFrom stringr str_c
#' @importFrom stringr str_c
check_exec <- function(exec, url="", package=exec, notes="", ...) {

  # find where the executable is installed
  exePath <- Sys.which(exec)

  # if if cannot be found
  if (exePath == "") {
    if (url != "") {
      # if a url is provided, point to it
      install <- str_c("See ", url, " for instructions")
    } else {
      # otherwise, try to provide some OS-specific installation instructions
      os <- guess_os()
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

    # in all cases, error-out while giving a (hopefully) informative message
    stop(exec, " is missing\n", install, "\n", notes, ...)
  }

  # return the full path to the executable
  return(exePath)
}


#' Check the exit status of a command (typically run through \code{system})
#'
#' @param status status code, usually an integer
#' @param message a character string with the message to print
#' @param ... passed to \code{stop}
#' @keywords internal
check_status <- function(status, message="discuss error or unexpected termination", ...) {
  if ( status != 0 ) {
    stop(message, call.=FALSE)
  }

  return(invisible(status))
}


#' Create a valid path from one or several path elements
#'
#' @param ... elements to be coerced as character strings and pasted together to make a path
#' @keywords internal
make_path <- function(...) {

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

  # make sure the path is valid and clean (perform path expansion, remove repeated path separators, etc.)
  path <- normalizePath(path, winslash="/", mustWork=FALSE)

  return(path)
}
