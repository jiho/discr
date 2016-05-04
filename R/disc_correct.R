#' Correct tracks (camera frame of reference) based on rotation of DISC to get polar-relevant coordinates
#'
#' @param dir path the to the deployment directory
#' @param max.time.difference maximum number of seconds allowed between successive positions for the computation of movement of the organism. When the time difference is larger than \code{max.time.difference}, \code{heading} and \code{speed} are NA. This should not be too large because movement over several seconds is rarely in a straight line and the heading and speed computed here would therefore be meaningless.
#' @param camera.compass.angle angle between the top of the camera and the N of the digital compass. If NULL, should be read from a file named "angle_camera_compass.txt" in the deployment directory
#' @param verbose output messages on the console when TRUE
#' @param ... passthrough argument
#'
#' @export
#' @family action functions
#'
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms
#' @importFrom plyr join
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#' deploy1 <- paste0(dd, "1")  # digital compass
#' deploy2 <- paste0(dd, "2")  # analog compass
#'
#' # run the action
#' disc_conf(deploy.dir=dd)
#' disc_correct(dir=deploy1, max.time.difference=5, verbose=TRUE)
#' # inspect results
#' list.files(deploy1)
#' head(read.csv(paste0(deploy1, "/larvae_tracks.csv")))
#' head(read.csv(paste0(deploy1, "/rotated_larvae_tracks.csv")))
#' tail(read.csv(paste0(deploy1, "/rotated_larvae_tracks.csv")))
#'
#' disc_correct(dir=deploy2, verbose=TRUE)
#' list.files(deploy2)
disc_correct <- function(dir, max.time.difference=2, camera.compass.angle=NULL, verbose=FALSE, ...) {

  disc_message("Correct rotation")

  # checks
  # must have tracks
  tracksFile <- make_path(dir, .files$tracks)
  assert_that(file.exists(tracksFile))

  # if digital compass is used, must have camera compass angle
  # either it is given on the command line or it is read from a file
  digitalCompassFile <- make_path(dir, .files$digital.compass)
  cameraCompassAngleFile <- make_path(dir, .files$camera.compass.angle)
  if ( file.exists(digitalCompassFile) & is.null(camera.compass.angle) ) {
    assert_that(file.exists(cameraCompassAngleFile))
  }

  # if analog compass is used, must have analog compass coords
  analogCompassFile <- make_path(dir, .files$analog.compass)
  analogCompassCoordFile <- make_path(dir, .files$analog.compass.coord)
  if ( file.exists(analogCompassFile) ) {
    assert_that(file.exists(analogCompassCoordFile))
  }


  # get options
  # aquarium diameter in cm
  diameter <- getOption("disc.diameter")

  ##{ Read and reformat larvae tracks ----

  if (verbose) { disc_message("read and process larvae tracks") }

  t <- read.csv(tracksFile)

  # suppress duplicate positions (two positions recorded for the same frame)
  # there should not be any but this can happen with the manual tracking plugin not working properly
  # when there are duplicates, keep the last recorded position
  t <- t[ ! duplicated(t[,c("trackNb", "imgNb")], fromLast=TRUE), ]

  # add time stamps to the tracks
  imgs <- read.csv(make_path(dir, str_c(.files$pictures, "_log.csv")))
  imgs$dateTime <- ymd_hms(imgs$dateTime)
  t <- join(t, imgs[,c("imgNb", "dateTime")], by="imgNb")

  # # Split larvae tracks in a list, one element per larva
  # tracks = split(trackLarva, trackLarva$trackNb)
  # nbTracks = length(tracks)
  
  # }

  ##{ Compute swimming speed, direction, turning angles etc. -----
  
  if (verbose) { disc_message("compute swimming characteristics") }

  # Compute swimming direction and speed
  t <- ddply(t, ~trackNb, function(x) {
    
    # compute displacement in x and y
    # = position at t+1 - position at t
    # the displacement is associated with time t (i.e. the last one is unknown)
    dx <- c(diff(x$x), NA)
    dy <- c(diff(x$y), NA)
    # and time difference between t and t+1
    dt <- diff(x$dateTime)
    units(dt) <- "secs" # force computation in seconds
    x$dt <- as.numeric(c(dt, NA))
    
    # compute elapsed time in minutes
    x$elapsed.min <- as.numeric(difftime(x$dateTime, x$dateTime[1], units="min"))
    
    # convert to polar coordinates (i.e. vector of displacement since last point)
    displacement <- car2pol(cbind(dx, dy), c(0,0))

    # store swimming heading in the appropriate circular class
    x$heading <- as.bearing(displacement$theta)
    # NB: this is heading in the reference of the aquarium

    # compute speed from displacement and interval
    x$speed <- displacement$rho / x$dt
    
    # remove movement data if time difference is too large
    large_steps <- x$dt > max.time.difference
    x$heading[large_steps] <- NA
    x$speed[large_steps]   <- NA

    return(x)
  })
  # }

  ##{ Read and reformat compass tracks ----

  if (verbose) { disc_message("read and process compass log") }

  # read compass record
  # it can either be a record from the numerical compass or from the backup, physical compass
  if ( file.exists(digitalCompassFile) ) {
    if (verbose) { disc_message("using digital compass") }

    # read camera compass angle
    if ( is.null(camera.compass.angle) ) {
      cameraCompassAngle <- as.bearing(dget(cameraCompassAngleFile))      
    } else {
      cameraCompassAngle <- camera.compass.angle
    }

    # read digital compass log
    compassLog <- read.csv(digitalCompassFile)
    compassLog$dateTime <- ymd_hms(compassLog$dateTime)
    compassLog$heading <- as.bearing(compassLog$heading)

    # correct heading of compass to be heading of the top of the frame
    # when looking up at the compass, we need to subtract the angle
    compassLog$cameraHeading <- compassLog$heading - cameraCompassAngle

  } else if ( file.exists(analogCompassFile) ) {

    if (verbose) { disc_message("using analog compass") }

    # read analog compass log
    compassLog <- read.csv(analogCompassFile)
    compassLog$dateTime <- ymd_hms(compassLog$dateTime)
    # NB: the upside-down orientation of the DISC is already taken into account here, and the cameraHeading column is already computed.

  } else {

    # no compass usable
    # just assume constant, 0, bearing from start to finish (i.e. no rotation)
    warning("no compass, assuming no rotation")

    dateRange <- range(t$dateTime)
    compassLog <- data.frame(dateTime=dateRange, cameraHeading=0)
    # NB: the 0 angle will then be interpolated at every point in the larva track

  }

  # make sure the camera heading has the appropriate class
  compassLog$cameraHeading <- as.bearing(compassLog$cameraHeading)

  # interpolate camera headings at every point in time in the tracks
  t$cameraHeading <- approx_circular(compassLog$dateTime, compassLog$cameraHeading, t$dateTime)$y

  # only keep larvae positions where a compass reading is available
  # might remove a few points at the beginning and end of the track, where the angle cannot be interpolated
  t <- t[ ! is.na(t$cameraHeading) , ]

  # }

  ##{ Compute larvae tracks in a cardinal reference ----

  if (verbose) { disc_message("compute larvae tracks in a cardinal reference") }

  # read calibration data
  coordAquarium <- read.csv(make_path(dir, .files$aquarium.coord))

  # correct for the rotation, putting the north upwards
  t <- ddply(t, ~trackNb, function(x) {

    # convert track to polar coordinates
    x[,c("theta","rho")] <- car2pol(x[,c("x","y")], c(coordAquarium$x, coordAquarium$y))

    # convert the angle to be measured as a bearing from the top of the frame
    x$theta <- as.bearing(x$theta)

    # reverse the direction of angles because we view the chamber from below
    x$theta <- from.below(x$theta)

    # correct for the rotation: add the angle in the chamber relative to the top of the frame to the heading of the top of the frame to find the true heading
    xCor <- x
    xCor$theta <- as.bearing(x$theta + x$cameraHeading)
    xCor$heading <- as.bearing(x$heading + x$cameraHeading)

    # remove speed from the rotated track
    xCor$speed <- NA

    # label the rotated data
    xCor$rotation <- "rotated"
    x$rotation <- "raw"
    # and combine the two
    x <- rbind(x, xCor)

    # compute turning angle
    # the angle associated with time t is the turn between the heading in the [t-1,t] and [t,t+1] intervals (=> unknown for t=0 and t=n)
    n <- nrow(x)
    x$turnAngle <- as.heading(c(NA, x$heading[2:n] - x$heading[1:(n-1)]))
    # NB: diff() does not work for circular objects

    # recompute cartesian positions from the polar definition
    x[,c("x","y")] <- pol2car(x[,c("theta","rho")])

    return(x)
  })

  # convert x, y, and rho to human significant measures (cm)
  # we use the diameter of the aquarium as a reference
  # we are given its value in cm and we have the perimeter, hence the diameter, in pixels
  px2cm <- diameter / ( coordAquarium$perim / pi )
  t[,c("x","y","speed","rho")] <- t[,c("x","y","speed","rho")] * px2cm

  # }

  ##{ Save tracks for statistical analysis and plotting ----

  # Write it to a csv file
  destFile <- make_path(dir, .files$rotated.tracks)
  if (verbose) { disc_message("write corrected track to ", destFile) }
  write.csv(t, file=destFile, row.names=F)
  
  # }
}
