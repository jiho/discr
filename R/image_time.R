# Extract time from EXIF data
#
# @param files full path to one or several image files
# @param tz force a time zone (most time computations are relative so getting the time zone right probably does not matter)
image_time <- function(files, tz="UTC") {
  # get date and times from EXIF data
  chunkSize <- 500
  dateTime <- c()
  for (i in seq(1, length(files), by=chunkSize)) {
    # message(i, " ", i+chunkSize-1)
    command <- stringr::str_c("exif -t=DateTimeOriginal -m \"", stringr::str_c(na.omit(files[i:(i+chunkSize-1)]), collapse="\" \""), "\"")
    # TODO use the 0x9291 or SubSecTimeOriginal tags too?
    dateTime <- c(dateTime, system(command, intern=TRUE))
  }

  # convert them to R representation
  dateTime <- lubridate::parse_date_time(dateTime, orders="ymdHMS", tz=tz)
  order <- order(dateTime)
  dateTime <- dateTime[order]

  # inspect time steps between images to detect large steps (camera shutdown) and reconstruct sub-seconds if necessary
  steps <- as.numeric(diff(dateTime))
  
  # detect sub-second intervals not actually recorded in the time stamps
  # = several 0 second intervals
  if (sum(steps == 0) > 1/10*length(dateTime)) {

    # detect steps over 1 s = probable camera shutdown
    # we need to remove them in the following
    large_steps <- steps > 1

    # take a guess for the actual time interval
    dt <- mean(steps[!large_steps], na.rm=TRUE)
    warning("Images have been taken at < 1 s intervals but split seconds were not recorded in the EXIF data.
  The average interval between images was ", round(dt, 3), " s and is used to reconstruct sub-second times.")

    # now guess time step and start time in each batch, between jumps
    d <- data.frame(dateTime, step=c(NA, steps), piece=c(0, cumsum(large_steps)))
    d <- plyr::ddply(d, ~piece, fit_t0_dt, dt=dt)
    dateTime <- d$dateTimeFixed
  }

  # put them back in the order of the files
  dateTime[order] <- dateTime

  return(dateTime)
}

# Given
# x data.frame with columns:
#   dateTime  date and time, full seconds
#   steps     steps between records in the dateTime column, in seconds
# dt initial estimate for the time step
# find the start time and time step (with sub-second resolution) that fit the full-second data best
fit_t0_dt <- function(x, dt) {
  # define all valid possibilities for the sub-second start and sub-second interval
  range_t0 <- c(0, 1)
  range_dt <- dt + c(-0.01, 0.01)
  # computethe start and end times to restrict to combinations that are valid
  startTime <- min(x$dateTime)
  endTime   <- max(x$dateTime)
  # define a grid to search for possibilities
  fineness <- 100
  grid <- data.frame()
  while ( nrow(grid) < fineness ) {
    grid <- expand.grid(
      t0=seq(range_t0[1], range_t0[2], length=fineness),
      dt=seq(range_dt[1], range_dt[2], length=ifelse(fineness %% 2 == 0, fineness+1, fineness))
      # NB: odd number allows to fit exactly the guessed value in case it is correct
    )
    # compute end time and remove points that do not fit the data
    ends <- (startTime + grid$t0) + (nrow(x)-1) * grid$dt
    grid <- grid[ends > endTime & ends < (endTime + 1),]
    # if none fit, refine the grid
    fineness <- round(fineness * 1.1)
  }
  # ggplot(grid) + geom_point(aes(x=dt, y=t0))

  # compute the match between steps
  score <- function(grid_line, observed_steps) {
    # recompute the rounded time vector
    t0 <- grid_line[1]
    dt <- grid_line[2]
    n <- length(observed_steps)
    x <- t0 + 0:n * dt
    x <- floor(x)
    # and steps
    recomputed_steps <- diff(x)
    # compute the number of mismatches
    score <- as.numeric(observed_steps != recomputed_steps)
    return(sum(score))
  }
  grid$score <- apply(grid, 1, score, observed_steps=x$step[-1])
  # ggplot(grid) + geom_raster(aes(x=dt, y=t0, fill=score))

  # keep only the appropriate values for the start time and time step
  grid <- grid[grid$score == min(grid$score, na.rm=TRUE),]
  # ggplot(grid) + geom_raster(aes(x=dt, y=t0, fill=score))
  fitted_t0 <- mean(grid$t0)
  fitted_dt <- mean(grid$dt)
  # message("t0=", fitted_t0, " dt=", fitted_dt)

  x$dateTimeFixed <- (startTime + fitted_t0) + seq(from=0, along=x$dateTime) * fitted_dt
  return(x)
}
