# Extract time from EXIF data
#
# @param img full path to one or several image files
# @param tz force a time zone (most time computations are relative so getting the time zone right probably does not matter)
#' @keywords internal
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
#' @importFrom plyr round_any
image_time <- function(img, tz="UTC") {
  # get date and times
  chunkSize <- 1000
  dateTime <- c()
  for (i in seq(1, length(img), by=chunkSize)) {
    # message(i, " ", i+chunkSize-1)
    command <- str_c("exif -t=DateTimeOriginal -m \"", str_c(na.omit(img[i:(i+chunkSize-1)]), collapse="\" \""), "\"")
    dateTime <- c(dateTime, system(command, intern=TRUE))
  }

  # convert them to R representations
  dateTime <- parse_date_time(dateTime, orders="ymd hms", tz=tz)

  return(dateTime)
}
