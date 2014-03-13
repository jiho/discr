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
