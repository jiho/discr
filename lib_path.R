join.path <- function(...) {
  args <- list(...)
  nArgs <- length(args)
  path <- args[[1]]
  if (nArgs > 1) {
    for (i in 2:nArgs) {
      path <- paste(path, args[[i]], sep="/")
    }
  }
  path <- normalizePath(path, winslash="/", mustWork=FALSE)
  return(path)
}
