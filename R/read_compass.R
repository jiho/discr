#' Compass reading
#'
#' Read data from various digital compass models and output a somewhat uniformised log
#'
#' @param file path to the file to read
#' @param ... passed to \code{\link{read.table}} which does the actual reading
#'
#' @return
#' A data.frame with columns: dateTime, heading, and other columns that depend on the compass model
#' @importFrom plyr rename
#' @importFrom plyr arrange
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
read_compass <- function(file, ...) {
  UseMethod("read_compass")
}

#' @rdname read_compass
#' @export
read_compass.ez <- function(file, ...) {
  # file <- "inst/tests/compass_ez_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Heading"="heading"))

  d$dateTime <- NA

  d <- reorder_columns(d, c("dateTime", "heading"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_compass
#' @export
#' @importFrom lubridate parse_date_time
read_compass.remora <- function(file, ...) {
  # file <- "inst/tests/compass_remora_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Date"="dateTime", "Heading"="heading"))

  d$dateTime <- parse_date_time(d$dateTime, orders="d-b-Y H:M:S", locale="en_US.UTF-8", quiet=TRUE)
  # NB: force english locale to make sure the month name is properly recognized

  # d <- reorder_columns(d, c("dateTime", "heading"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_compass
#' @export
read_compass.opentag <- function(file, ...) {
  # file <- "inst/tests/compass_opentag_sample.csv"

  # read and subsample the data (because we don't need one reading every millisecond)
  d <- read.csv(file, stringsAsFactors=FALSE, ...)
  d <- d[seq(1, nrow(d), by=50),]
  # NB: using scan and subsamping afterwards or using sed is actually not faster (strange though...)

  # homogenise output
  # d <- rename(d, c("Heading"="heading"))

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms", quiet=TRUE)
  options(digits.secs=3)
  d$dateTime <- d$dateTime + d$Time.from.Start..s.
  d <- subset(d, select=-c(FileDate, FileTime, Time.from.Start..s., X))
  # NB: the X column is empty and useless
  # TODO compute the heading from the magnetic recordings etc.

  d <- reorder_columns(d, c("dateTime"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_compass
#' @export
read_compass.cc <- function(file, ...) {
  # file <- "inst/tests/compass_cc_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("date"="dateTime"))

  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)

  # d <- reorder_columns(d, c("dateTime", "heading"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}
