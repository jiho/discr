# Read a CSV file and automatically detect CSV convention (, or ; )
#
# NB: we could be using data.table::fread directly but that would add a dependency
read_csv_auto <- function(file, ...) {
  # scan the first line and count ; or ,
  line <- scan(file, sep="\n", what="character", n=1, quiet=T)
  n_semi <- length(stringr::str_split(line, stringr::fixed(";"))[[1]])
  n_comma <- length(stringr::str_split(line, stringr::fixed(","))[[1]])
  
  # choose the appropriate function
  if (n_semi > n_comma) {
    read_csv <- read.csv2
  } else {
    read_csv <- read.csv
  }
  read_csv(file=file, ...)
}
