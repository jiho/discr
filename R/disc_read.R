#
# Functions to read the raw data from each leg of deployment
# Each function should return a data.frame with at least a "dateTime" column
#

#' Raw data read
#'
#' Read data from various sensors output somewhat uniformised records
#'
#' @param file path to the file to read
#' @param ... passed to \code{\link{read.table}} which does the actual reading
#'
#' @return
#' A data.frame with a columns \code{dateTime} and other columns that depend on the sensor
#' @importFrom plyr rename
#' @importFrom plyr arrange
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
disc_read <- function(file, ...) {
  UseMethod("disc_read")
}



## GPS

#' @rdname disc_read
#' @export
disc_read.gt31 <- function(file, ...) {
  # file <- "inst/tests/gps_gt31_sample.plt"

  d <- read.table(file, skip=6, stringsAsFactors=FALSE, sep=",", strip.white=TRUE, col.names=c("lat", "lon", "?", "?", "?", "date", "time"), ...)

  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="dmy hms", quiet=TRUE)

  d <- select_columns(d, c("dateTime", "lon", "lat"))

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.igotu <- function(file, ...) {
  # file <- "inst/tests/gps_igotu_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Latitude"="lat", "Longitude"="lon"))

  d$dateTime <- str_c(d$Date, " ", d$Time)
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)
  d <- remove_columns(d, c("Date", "Time"))

  d <- reorder_columns(d, c("dateTime", "lon", "lat"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.trackstick <- function(file, ...) {
  # file <- "inst/tests/gps_trackstick_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Latitude"="lat", "Longitude"="lon"))

  d$dateTime <- parse_date_time(d$Date, orders="m/d/Y H:M", quiet=TRUE)
  d <- remove_columns(d, "Date")

  d <- reorder_columns(d, c("dateTime", "lon", "lat"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}


## CTD

#' @rdname disc_read
#' @export
#' @importFrom stringr str_detect
disc_read.dst <- function(file, ...) {
  # file <- "inst/tests/ctd_dst_sample.dat"

  content <- scan(file, what="character", sep="\n", quiet=TRUE)
  header <- which(suppressWarnings(str_detect(content, "^#")))
  d <- read.table(file, skip=max(header), stringsAsFactors=FALSE, col.names=c("recordNb", "date", "time", "temperature", "depth", "salinity"), ...)

  # homogenise output
  # d <- rename(d, c("Heading"="heading"))

  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms")
  d <- remove_columns(d, c("date", "time"))

  d <- reorder_columns(d, c("dateTime", "depth", "temperature"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.opentag <- function(file, ...) {
  # file <- "inst/tests/ctd_opentag_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE)

  # homogenise output
  d <- rename(d, c("Pressure"="depth", "Temperature"="temperature"))

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms", quiet=TRUE)
  d$dateTime <- d$dateTime + d$Time.from.Start..s.
  d <- remove_columns(d, c("FileDate", "FileTime", "Time.from.Start..s.", "X"))
  # NB: the X column is empty and useless

  d <- reorder_columns(d, c("dateTime", "depth", "temperature"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}


## COMPASS

#' @rdname disc_read
#' @export
disc_read.ez <- function(file, ...) {
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

#' @rdname disc_read
#' @export
#' @importFrom lubridate parse_date_time
disc_read.remora <- function(file, ...) {
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

#' @rdname disc_read
#' @export
disc_read.opentag <- function(file, ...) {
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
  d <- remove_columns(d, c("FileDate", "FileTime", "Time.from.Start..s.", "X"))
  # NB: the X column is empty and useless
  # TODO compute the heading from the magnetic recordings etc.

  d <- reorder_columns(d, c("dateTime"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.cc <- function(file, ...) {
  # file <- "inst/tests/compass_cc_sample.TXT"

  d <- read.csv(file, stringsAsFactors=FALSE, col.names=c("dateTime", "pitch", "roll", "heading", "junk"), ...)
  d <- remove_columns(d, "junk")

  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)

  return(d)
}


#' @rdname disc_read
#' @export
disc_read.hobo <- function(file, ...) {
  # file <- "inst/tests/hobo_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, skip=1, ...)
  
  d <- d[,2:4]
  names(d) <- c("dateTime", "temp", "light")

  d$dateTime <- parse_date_time(d$dateTime, orders="dmy Ims p", quiet=TRUE)

  return(d)
  
}