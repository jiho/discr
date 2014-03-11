#
#      Handle disc-specific options
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------

disc_read_options <- function(file="disc.conf", verbose=FALSE) {
  #
  # Read disc-specific options from the config file
  #
  # file    config file, must contain the definition of an R list with options
  # verbose if TRUE, show options on the console

  # read disc specific options
  if (file.exists(file)) {
    opts.disc <- dget(file)
  } else {
    opts.disc <- list()
  }

  # print them if requested
  if (verbose) {
    message("DISC options")
    print(opts.disc)
  }

  # store them as global R options to avoid passing them around in functions or using a global variable
  op <- options()
  toset <- !(names(opts.disc) %in% names(op))
  if(any(toset)) options(opts.disc[toset])

  return(opts.disc)
}

disc_write_options <- function(opts, file="disc.conf") {
  #
  # Write disc-specific options in a configuration file
  #
  # opts  disc options
  # file  config file

  # # transform options list into a character string representation
  # opStr <- deparse(opts, control=NULL, width.cutoff=500)
  #
  # # reformat it to be human readable
  # suppressPackageStartupMessages(require("stringr", quietly=TRUE))
  # opStr <- str_c(opStr, collapse="")
  # opStr <- str_replace(opStr, ")$", "\n)\n")
  # opStr <- str_replace_all(opStr, "disc", "\n\tdisc")
  #
  # # write it in the options file
  # cat(opStr, file=file)

  dput(opts, file=file)

  return(invisible(file))
}
