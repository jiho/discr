# Create a valid path from one or several path elements
#
# @param ... elements to be coerced as character strings and pasted together to make a path
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
