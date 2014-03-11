#' Create a valid path from one or several path elements
#'
#' @param ... elements to be coerced as character strings and pasted together to make a path
#' @keywords internal
make_path <- function(...) {

  # get all arguments
  args <- list(...)

  # paste them together
  path <- paste(args, collapse="/")

  # make sure the path is valid and clean (perform path expansion, remove repeated path separators, etc.)
  path <- normalizePath(path, winslash="/", mustWork=FALSE)

  return(path)
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
