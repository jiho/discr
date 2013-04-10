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
