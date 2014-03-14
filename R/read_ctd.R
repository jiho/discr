#' CTD reading
#'
#' Read data from various CTD models and output a somewhat uniformised log
#'
#' @param file path to the file to read
#' @param ... passed to \code{\link{read.table}} which does the actual reading
#'
#' @return
#' A data.frame with columns: dateTime, heading, and other columns that depend on the ctd model
#' @importFrom plyr rename
#' @importFrom plyr arrange
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
read_ctd <- function(file, ...) {
  UseMethod("read_ctd")
}

#' @rdname read_ctd
#' @export
#' @importFrom stringr str_detect
read_ctd.dst <- function(file, ...) {
  # file <- "inst/tests/ctd_dst_sample.dat"

  content <- scan(file, what="character", sep="\n", quiet=TRUE)
  header <- which(suppressWarnings(str_detect(content, "^#")))
  d <- read.table(file, skip=max(header), stringsAsFactors=FALSE, col.names=c("recordNb", "date", "time", "temperature", "depth", "salinity"), ...)

  # homogenise output
  # d <- rename(d, c("Heading"="heading"))

  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms")
  d <- subset(d, select=c(-date, -time))

  d <- reorder_columns(d, c("dateTime", "depth", "temperature"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_ctd
#' @export
read_ctd.opentag <- function(file, ...) {
  # file <- "inst/tests/ctd_opentag_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE)

  # homogenise output
  d <- rename(d, c("Pressure"="depth", "Temperature"="temperature"))

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms", quiet=TRUE)
  d$dateTime <- d$dateTime + d$Time.from.Start..s.
  d <- subset(d, select=-c(FileDate, FileTime, Time.from.Start..s., X))
  # NB: the X column is empty and useless

  d <- reorder_columns(d, c("dateTime", "depth", "temperature"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}
