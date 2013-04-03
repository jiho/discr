join.path <- function(...) {
  args <- list(...)
  path <- args[[1]]
  for (i in 2:length(args)) {
    path <- paste(path, args[[i]], sep="/")
  }
  path <- normalizePath(path, winslash="\\", mustWork=FALSE)
  return(path)
}