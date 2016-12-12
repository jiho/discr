#' Raw data reading
#'
#' Read data from various sensors and output somewhat uniformed records.
#'
#' @param dir path to the directory containing the files to read.
#' @param ... passed to other functions that do the actual reading, most often \code{\link[utils]{read.table}}.
#'
#' @details
#' All methods of this generic function should takes the path to a directory (where the data is stored) as input and give a data.frame as output, with at least a column called \code{dateTime} of class \code{\link[base]{POSIXct}} containing the date and time of that record. This column will be used to shift the data by a user configured offset (in \code{\link{disc_extract_deployments}}) and synchronise sensors. The rest of the columns depend on the sensors. The only constraint is for compass-type data to have a column named \code{heading}.
#' 
#' For instruments that output binary data (videos, audio files, etc.), the \code{disc_read} method should provide time stamps and paths to the various files. This information is then used by the appropriate \code{disc_extract} method to actually move/split/etc. the leg-level binary file into a file for each deployment.
#'
#' @seealso \code{\link{disc_extract}} and \code{\link{disc_extract_deployments}}
#'
#' @importFrom dplyr rename arrange select
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
disc_read <- function(dir, ...) {
  UseMethod("disc_read")
}



## GPS ----

#' @rdname disc_read
#' @export
disc_read.gt31 <- function(dir, ...) {
  # read .plt files = some sort of text log from the GT31
  files <- list.files(dir, pattern=glob2rx("*.plt"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- plyr::ldply(files, read.table, skip=6, stringsAsFactors=FALSE, sep=",", strip.white=TRUE, col.names=c("lat", "lon", "?", "?", "?", "date", "time"), ...)

  # compute date+time for R
  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="dmy HMS", quiet=TRUE)

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
  d <- plyr::ldply(files, read.csv, stringsAsFactors=FALSE, ...)

  d$dateTime <- str_c(d$Date, " ", d$Time)
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd HMS", quiet=TRUE)
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
  d <- rename(d, lat=Latitude, lon=Longitude)

  d$dateTime <- parse_date_time(d$Date, orders="m/d/Y H:M", quiet=TRUE)
  d <- select(d, -Date)

  d <- select(d, dateTime, lon, lat)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.gw52 <- function(dir, ...) {
  # read .csv files = converted .sbp files from http://www.gpsvisualizer.com/gpsbabel/gpsbabel_convert
  files <- list.files(dir, pattern=glob2rx("*.csv"), full.names=TRUE)
  
  # there should be only one per directory, but just in case, loop automatically over all files
  d <- plyr::ldply(files, read.csv, stringsAsFactors=FALSE)
  
  # parse date and time
  d$dateTime <- parse_date_time(str_c(d$DATE, " ", d$TIME), orders="ymd HMS")
  
  # keep only relevant data
  d <- select(d, dateTime, lon=LONGITUDE, lat=LATITUDE, speed=SPEED)
  
  return(d)
}


## CTD ----

#' @rdname disc_read
#' @export
#' @importFrom stringr str_detect
disc_read.dst <- function(dir, ...) {
  # file <- "inst/tests/ctd_dst_sample.dat"

  content <- scan(file, what="character", sep="\n", quiet=TRUE)
  header <- which(suppressWarnings(str_detect(content, "^#")))
  d <- read.table(file, skip=max(header), stringsAsFactors=FALSE, col.names=c("recordNb", "date", "time", "temperature", "depth", "salinity"), ...)

  # homogenise output
  # d <- rename(d, heading=Heading)

  d$dateTime <- str_c(d$date, " ", d$time)
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy HMS")
  d <- select(d, -date, -time)

  d <- select(d, dateTime, depth, temperature)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.dstctd <- function(dir, ...) {
  # read .csv files = converted .xlsx files from star-oddi seastar software
  files <- list.files(dir, pattern=glob2rx("*.csv"), full.names=TRUE)
  
  # there should be only one per directory, but just in case, loop automatically over all files
  d <- plyr::ldply(files, read.csv, stringsAsFactors=FALSE)
  
  # convert the datetime numeric to something meaningful
  d$dateTime <- as.POSIXct(d$Date...Time, format="%m/%d/%Y %H:%M:%S")
  
  # rename headers
  colnames(d) <- c("dateTime", "temp", "depth","salinity")
  
  return(d)
}


#' @rdname disc_read
#' @export
disc_read.ctd_opentag <- function(dir, ...) {
  # file <- "inst/tests/ctd_opentag_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE)

  # homogenise output
  d <- rename(d, depth=Pressure, temperature=Temperature)

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy HMS", quiet=TRUE)
  d$dateTime <- d$dateTime + d$Time.from.Start..s.
  d <- select(d, -FileDate, -FileTime, -Time.from.Start..s.)

  d <- select(d, dateTime, depth, temperature)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}


## COMPASS ----

#' @rdname disc_read
#' @export
disc_read.ez <- function(dir, ...) {
  # file <- "inst/tests/compass_ez_sample.csv"

  d <- read.csv(file, stringsAsFactors=FALSE, ...)

  # homogenise output
  d <- rename(d, heading=Heading)

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
  d <- rename(d, dateTime=Date, heading=heading)

  d$dateTime <- parse_date_time(d$dateTime, orders="d-b-Y H:M:S", locale="en_US.UTF-8", quiet=TRUE)
  # NB: force english locale to make sure the month name is properly recognized

  d <- select(d, dateTime, heading)

  # sort by time
  d <- arrange(d, dateTime)

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.compassOpentag <- function(dir, ...) {
  # file <- "inst/tests/compass_opentag_sample.csv"

  # read and subsample the data (because we don't need one reading every millisecond)
  d <- read.csv(file, stringsAsFactors=FALSE, ...)
  d <- d[seq(1, nrow(d), by=50),]
  # NB: using scan and subsamping afterwards or using sed is actually not faster (strange though...)

  # homogenise output
  # d <- rename(d, heading=Heading)

  d$dateTime <- str_c(d$FileDate, d$FileTime, sep=" ")
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy HMS", quiet=TRUE)
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
  d <- plyr::ldply(files, read.csv, stringsAsFactors=FALSE, col.names=c("dateTime", "pitch", "roll", "heading", "light"), ...)

  # compute date+time for R
  d$dateTime <- parse_date_time(d$dateTime, orders="ymd HMS", quiet=TRUE)

  # remove light if completely empty (early versions of the CC)
  if (all(is.na(d$light))) {
    d <- select(d, -light)
  }

  return(d)
}


## Light sensors ----

#' @rdname disc_read
#' @export
disc_read.hobo <- function(dir, ...) {
  # read csv files files
  files <- list.files(dir, pattern=glob2rx("*.csv"), full.names=TRUE)

  # there should be only one per directory, but just in case, loop automatically over all files
  d <- plyr::ldply(files, read.csv, stringsAsFactors=FALSE, skip=1, ...)

  # keep only appropriate columns and label them
  d <- d[,2:4]
  names(d) <- c("dateTime", "temp", "light")

  # compute date+time for R
  d$dateTime <- parse_date_time(d$dateTime, orders="mdy Ims p", quiet=TRUE)

  return(d)
}


## Pictures and video ----

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

  # detect camera shutdown (step of > 30sec)
  steps <- diff(d$dateTime)
  large_steps <- steps > 30
  # warn about them
  if (any(large_steps)) {
    large_steps_indexes <- which(large_steps)
    large_steps_times <- plyr::laply(large_steps_indexes, function(x) {
      stringr::str_c(dateTime[x:(x+1)], collapse=" -> ")
    })
    warning("The camera did not record data between:\n  ", stringr::str_c(large_steps_times, collapse="\n  "), "\n  Was this expected?")
  }

  return(d)
}

#' @rdname disc_read
#' @export
disc_read.goproVideo <- function(dir, ...) {
  # get all MP4 files
  # NB: GoPros cut files in ~ 21 mins portions
  files <- list.files(dir, pattern=glob2rx("G*.MP4"), full.names=TRUE, recursive=TRUE)

  # get start time for each file
  d <- plyr::ldply(files, function(file) {
    out <- system2("ffprobe", str_c("-select_streams v:0 -print_format csv -show_entries stream=duration:stream_tags=creation_time \"", file, "\""), stdout=TRUE, stderr=FALSE)
    out <- read.csv(text=out, header=F, col.names=c("stream", "duration", "begin"))
    out$file <- file
    return(out)
  })
  
  # remove the stream column
  d <- dplyr::select(d, -stream)
  
  # convert into POSIXct
  d$begin <- parse_date_time(d$begin, orders="ymd HMS")
  
  # compute end time
  d$end <- d$begin + d$duration
  
  # gather start and end in one column
  d <- tidyr::gather(d, key="type", value="dateTime", begin, end)

  # and order by time
  d <- arrange(d, dateTime, type)

  return(d)
}

