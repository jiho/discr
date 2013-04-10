disc_read_options <- function(file="disc.conf", quiet=TRUE) {
  # read disc specific options
  if (file.exists(file)) {
    opDisc <- dget(file)
  } else {
    opDisc <- list()
  }

  if (!quiet) {
    message("DISC options")
    print(opDisc)
  }

  # store them as global R options to avoid passing them around in functions
  op <- options()
  toset <- !(names(opDisc) %in% names(op))
  if(any(toset)) options(opDisc[toset])

  return(opDisc)
}

disc_write_options <- function(op, file="disc.conf") {
  # # transform options list into a character string representation
  # opStr <- deparse(op, control=NULL, width.cutoff=500)
  # 
  # # reformat it to be human readable
  # suppressPackageStartupMessages(require("stringr", quietly=TRUE))
  # opStr <- str_c(opStr, collapse="")
  # opStr <- str_replace(opStr, ")$", "\n)\n")
  # opStr <- str_replace_all(opStr, "disc", "\n\tdisc")
  # 
  # # write it in the options file
  # cat(opStr, file=file)

  dput(op, file=file)
  
  return(invisible(file))
}
