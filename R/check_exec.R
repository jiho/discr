# Check the existence of an executable
#
# Check that a given executable can be found in the path and return its full path. If it cannot be found, provide some installation instructions
#
# @param exec name of the executable
# @param url link to point the user to when the executable is absent
# @param package name of the package usually providing this executable, used to indicate how to install the software when url is not provided
# @param notes further notes appended to the error message
# @param error.out \code{check_exec()} emits a warning by default but produces an error when this is true
# @param ... passed to \code{link[base]{warning}}
#
#' @importFrom stringr str_c
check_exec <- function(exec, url="", package=exec, notes="", error.out=FALSE, ...) {

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
        install <- str_c("Install homebrew: http://brew.sh/\n",
                         "Then, install ", package, " with: brew install ", package)
      } else if (os == "debian-like") {
        install <- str_c("Install it with: sudo apt-get install ", package)
      } else if (os == "redhat-like") {
        install <- str_c("Install it with: sudo yum install ", package)
      } else if (os == "suse-like") {
        install <- str_c("Install it with: sudo zypper install ", package)
      } else if (os == "windows") {
        install <- str_c("Install Cygwin: https://www.cygwin.com (probably x86_64) with the default options.\nAt the package selection step, select package \"", package, "\" (in Graphics).\nThe first time you run it, go to Control Panel > System > Advanced System Settings > Environment Variables > System Variables, search for the `Path` variable, click `Modify` and preprend \"c:\\cygwin64\\bin;\" to it. Restart R.")
      } else {
        install <- str_c("Search your package manager for ", package)
      }
    }

    # in all cases, give a (hopefully) informative message
    # either as an error or as an immediate warning
    warnOption <- getOption("warn")
    if ( error.out ) {
      options("warn"=2)
    } else {
      options("warn"=1)
    }
    warning(exec, " is missing\n", install, "\nNote: ", notes, ...)
    options("warn"=warnOption)  # reset it to what it was
  } else {
    message(str_c(exec, " executable found"))
  }

  # return the full path to the executable
  return(invisible(exePath))
}
