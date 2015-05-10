#
# Functions to read the raw data from each leg of deployment
# Each function should return a data.frame with at least a "dateTime" column
#

#' Raw data reading
#'
#' Read data from various sensors output somewhat uniformed records
#'
#' @param dir path to the directory in which the files to read are
#' @param ... passed to \code{\link[utils]{read.table}} which does the actual reading
#'
#' @details
#' \code{\link{disc_extract_deployments}} reads data from all sensors defined in the leg log file. To do so it looks for an appropriate method for the generic function \code{disc_read}, i.e. a function named \code{disc_read.nameofsensor}. Do read data from a new sensor, one just need to define such as function, which takes the path to a directory (where the data is stored) as input and gives a data.frame as output, with at least a column called "\code{dateTime}" holding the date and time in the default format for \code{link[base]{as.POSIXct}} (i.e. "YYYY-MM-DD HH:MM:SS"). The rest of the columns depend on the sensors. The only constraint is for compass-type data to have a column named "\code{heading}"
#'
#' @importFrom plyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
disc_read <- function(dir, ...) {
  UseMethod("disc_read")
}



## GPS

#' @rdname disc_read
#' @export
disc_read.gt31 <- function(dir, ...) {
  # read .plt files = some sort of text log from the GT31
  files <- list.files(dir, pattern=glob2rx("*.plt"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- ldply(files, read.table, skip=6, stringsAsFactors=FALSE, sep=",", strip.white=TRUE, col.names=c("lat", "lon", "?", "?", "?", "date", "time"), ...)

  # compute date+time for R
  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="dmy hms", quiet=TRUE)

  # keep only relevant data
  d <- select(d, dateTime, lon, lat)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.igotu <- function(dir, ...) {
  # dir <- "inst/tests/igotu"

  files <- list.files(dir, pattern=glob2rx("*.csv"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- ldply(files, read.csv, stringsAsFactors=FALSE, ...)

  d$dateTime <- str_c(d$Date, " ", d$Time)
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)
  d <- select(d, -Date, -Time)

  d <- select(d, dateTime, lon=Latitude, lat=Longitude)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.trackstick <- function(dir, ...) {
  # file <- "inst/tests/gps_trackstick_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Latitude"="lat", "Longitude"="lon"))

  d$dateTime <- parse_date_time(d$Date, orders="m/d/Y H:M", quiet=TRUE)
  d <- select(d, -Date)

  d <- select(d, dateTime, lon, lat)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}


## CTD

#' @rdname disc_read
#' @export
#' @importFrom stringr str_detect
disc_read.dst <- function(dir, ...) {
  # file <- "inst/tests/ctd_dst_sample.dat"

  content <- scan(file, what="character", sep="\n", quiet=TRUE)
  header <- which(suppressWarnings(str_detect(content, "^#")))
  d <- read.table(file, skip=max(header), stringsAsFactors=FALSE, col.names=c("recordNb", "date", "time", "temperature", "depth", "salinity"), ...)

  # homogenise output
  # d <- rename(d, c("Heading"="heading"))

  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms")
  d <- select(d, -date, -time)

  d <- select(d, dateTime, depth, temperature)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.ctd_opentag <- function(dir, ...) {
  # file <- "inst/tests/ctd_opentag_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE)

  # homogenise output
  d <- rename(d, c("Pressure"="depth", "Temperature"="temperature"))

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy hms", quiet=TRUE)
  d$dateTime <- d$dateTime + d$Time.from.Start..s.
  d <- select(d, -FileDate, -FileTime, -Time.from.Start..s.)

  d <- select(d, dateTime, depth, temperature)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}


## COMPASS

#' @rdname disc_read
#' @export
disc_read.ez <- function(dir, ...) {
  # file <- "inst/tests/compass_ez_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Heading"="heading"))

  d$dateTime <- NA

  d <- select(d, dateTime, heading)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
#' @importFrom lubridate parse_date_time
disc_read.remora <- function(dir, ...) {
  # file <- "inst/tests/compass_remora_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, c("Date"="dateTime", "Heading"="heading"))

  d$dateTime <- parse_date_time(d$dateTime, orders="d-b-Y H:M:S", locale="en_US.UTF-8", quiet=TRUE)
  # NB: force english locale to make sure the month name is properly recognized

  d <- select(d, dateTime, heading)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.compass_opentag <- function(dir, ...) {
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
  d <- select(d, -FileDate, -FileTime, -Time.from.Start..s.)
  # TODO compute the heading from the magnetic recordings etc.

  d <- select(d, dateTime, heading)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.cc <- function(dir, ...) {
  # read DATALOG.TXT files
  files <- list.files(dir, pattern=glob2rx("DATALOG.TXT"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- ldply(files, read.csv, stringsAsFactors=FALSE, col.names=c("dateTime", "pitch", "roll", "heading", "junk"), ...)

  # compute date+time for R
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd hms", quiet=TRUE)

  # keep only relevant data
  d <- select(d, -junk)

  return(d)
}


#' @rdname disc_read
#' @export
disc_read.hobo <- function(dir, ...) {
  # read csv files files
  files <- list.files(dir, pattern=glob2rx("*.csv"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- ldply(files, read.csv, stringsAsFactors=FALSE, skip=1, ...)

  # keep only appropriate columns and label them
  d <- d[,2:4]
  names(d) <- c("dateTime", "temp", "light")

  # compute date+time for R
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy Ims p", quiet=TRUE)

  return(d)

}


#' @rdname disc_read
#' @export
disc_read.gopro <- function(dir, ...) {
  # get all JPG files
  files <- list.files(dir, pattern=glob2rx("G00*.JPG"), full.names=TRUE, recursive=TRUE)

  # get timestamps for theses files
  timestamps <- image_time(files)

  # store that in a data.frame
  d <- data.frame(origFile=files, dateTime=timestamps, stringsAsFactors=FALSE)
  # make sure it is ordered by time (listing files can be done in non chronological order)
  d <- d[order(d$dateTime),]

  return(d)
}