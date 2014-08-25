# Write a progress message on the console
#
# This is similar to \code{message} except it will add nice indentation and won't be bold
#
# @param ... message components passed to \code{cat}
#
#' @importFrom stringr str_c
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_length
disc_message <- function(...) {
  # concatenate the message bits
  message <- str_c(..., sep="")

  # compute how many lines it will take
  nCharacters <- str_length(message)
  indentLength <- 2
  lineLength <- 71 - indentLength
  nLines <- nCharacters %/% lineLength + 1
  
  # write every line
  for (i in 1:nLines) {
    thisLine <- str_sub(message, (i-1)*lineLength+1, i*lineLength)
    thisLine <- str_trim(thisLine)
    cat(rep(" ", indentLength), thisLine, "\n", sep="")
  }
  
  return(invisible(NULL))
}

# @rdname disc_message
dmessage <- disc_message

# @rdname disc_message
dmess <- disc_message
