# Extract time from EXIF data
#
# @param img full path to one or several image files
# @param tz force a time zone (most time computations are relative so getting the time zone right probably does not matter)
image_time <- function(img, tz="UTC") {
  # get date and times
  chunkSize <- 500
  dateTime <- c()
  for (i in seq(1, length(img), by=chunkSize)) {
    # message(i, " ", i+chunkSize-1)
    command <- stringr::str_c("exif -t=DateTimeOriginal -m \"", stringr::str_c(na.omit(img[i:(i+chunkSize-1)]), collapse="\" \""), "\"")
    # TODO use the 0x9291 or SubSecTimeOriginal tags too?
    dateTime <- c(dateTime, system(command, intern=TRUE))
  }

  # convert them to R representations
  dateTime <- lubridate::parse_date_time(dateTime, orders="ymd hms", tz=tz)

  # resolve split seconds
  steps <- as.numeric(diff(dateTime))
  # if many steps are 0, then the delay is probably < 1 sec and the camera does not record those in EXIF data
  if (sum(steps == 0) > 1/10*length(dateTime)) {
    options("digits.secs"=2)
    # detect and remove large steps = probable camera shutdown
    large_steps <- steps > 1
    # compute the actual time step
    (true_step <- mean(steps[!large_steps]))

    warning("Images seem to have been taken at < 1 s intervals but split seconds were not recorded in the images' EXIF data. They are being reconstructed assuming the actual interval between images was: ", true_step, " s.")

    n_large_steps <- sum(large_steps)
    if (n_large_steps > 0) {
      warning("In addition, ", n_large_steps, " event(s) was/were detected during which the camera did not take images for more than 1 s. Those situations are assumed to be pauses in sampling and split-seconds reconstruction omits them.")
    }
    
    # now process the data in batches between the jumps
    pieces <- c(0, cumsum(large_steps))
    d <- data.frame(dateTime, pieces)
    d <- ddply(d, ~pieces, function(x) {
      x$dateTime <- x$dateTime[1] + seq(from=0, along=x$dateTime) * true_step
      return(x)
    })
    dateTime <- d$dateTime
  }
  
  return(dateTime)
}
