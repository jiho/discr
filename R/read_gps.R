#' GPS reading
#'
#' Read data from various GPS model and output a somewhat uniformised log
#'
#' @param file path to the file to read
#' @param ... passed to \code{\link{read.table}} which does the actual reading
#'
#' @return
#' A data.frame with columns: dateTime, lon, lat, and other columns that depend on the GPS model
#' @importFrom plyr rename
#' @importFrom plyr arrange
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
read_gps <- function(file, ...) {
  UseMethod("read_gps")
}

#' @rdname read_gps
#' @export
read_gps.gt31 <- function(file, ...) {
  # file <- "inst/tests/gps_gt31_sample.csv"

  d <- read.csv(file, skip=1, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Lat"="lat", "Long"="lon"))

  d$dateTime <- str_c(d$Year, "-", d$Month, "-", d$Day, " ", d$Hour, ":", d$Min, ":", d$Sec)
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)
  d <- subset(d, select=-c(Year, Month, Day, Hour, Min, Sec))

  d <- reorder_columns(d, c("dateTime", "lon", "lat"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_gps
#' @export
read_gps.igotu <- function(file, ...) {
  # file <- "inst/tests/gps_igotu_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Latitude"="lat", "Longitude"="lon"))

  d$dateTime <- str_c(d$Date, " ", d$Time)
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)
  d <- subset(d, select=-c(Date, Time))

  d <- reorder_columns(d, c("dateTime", "lon", "lat"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname read_gps
#' @export
#' @importFrom lubridate parse_date_time
read_gps.trackstick <- function(file, ...) {
  # file <- "inst/tests/gps_trackstick_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Latitude"="lat", "Longitude"="lon"))

  d$dateTime <- parse_date_time(d$Date, orders="m/d/Y H:M", quiet=TRUE)
  d <- subset(d, select=-c(Date))

  d <- reorder_columns(d, c("dateTime", "lon", "lat"))

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}
