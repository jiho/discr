# Extract time from EXIF data
#
# @param img full path to one or several image files
# @param tz force a time zone (most time computations are relative so getting the time zone right probably does not matter)
#' @keywords internal
#' @importFrom stringr str_c
#' @importFrom lubridate parse_date_time
image_time <- function(img, tz="UTC") {
  # get date and times
  dateTime <- system(str_c("exif -t=DateTimeOriginal -m ", str_c(img, collapse=" ")), intern=TRUE)

  # convert them to R representations
	dateTime <- parse_date_time(dateTime, format="ymd hms", tz=tz)

	return(dateTime)
}


# Compute the average interval in full seconds between provided images
#
# @param img vector of full paths to images
#' @keywords internal
image_interval <- function(img) {

	if ( sum(file.exists(img)) >= 2 ) {
		# detect image times
		times <- image_time(img)

		# compute intervals in seconds
		intervals <- diff(times)
		units(intervals) <- "secs"

    # compute mean interval in whole seconds
		interval <- as.integer( round( mean(intervals) ) )

	} else {
		warning("At least two images are needed to compute a time interval")
    interval <- NULL
	}

	return(interval)
}
