# Guess the operating system
#
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
