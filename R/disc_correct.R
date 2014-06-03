#' Correct tracks (camera frame of reference) based on rotation of DISC to get polar-relevant coordinates
#'
#' @param dir path the to the deployment directory
#' @param verbose output messages on the console when TRUE
#'
#' @export
#'
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms
#' @importFrom plyr join
disc_correct <- function(dir, verbose=FALSE, ...) {

  disc_message("Correct rotation")

  # get options
  # wether the camera is looking up or down on the chamber
  looking_up <- getOption("disc.looking_up")
  # aquarium diameter in cm
  diameter <- getOption("disc.diameter")

  # Read and reformat larvae tracks
  #--------------------------------------------------------------------------
  if (verbose) { disc_message("read and process larvae tracks") }

  t <- read.csv(make_path(dir, .files$tracks))

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


  # Read and reformat compass tracks
  #--------------------------------------------------------------------------
  if (verbose) { disc_message("read and process compass log") }

  digitalCompassFile <- make_path(dir, .files$digital.compass)
  analogCompassFile <- make_path(dir, .files$analog.compass)
  analogCompassCoordFile <- make_path(dir, .files$analog.compass.coord)

  # read compass record
  # it can either be a record from the numerical compass or from the backup, physical compass
  if ( file.exists(digitalCompassFile) ) {
    if (verbose) { disc_message("using digital compass") }

    # read camera compass angle
    cameraCompassAngle <- as.bearing(dget(make_path(dir, .files$camera.compass.angle)))

    # read digital compass log
    compassLog <- read.csv(digitalCompassFile)
    compassLog$dateTime <- ymd_hms(compassLog$dateTime)
    compassLog$heading <- as.bearing(compassLog$heading)

    # correct heading of compass to be heading of the top of the frame
    # the correction depends on the position of the camera
    if ( looking_up ) {
      compassLog$cameraHeading <- compassLog$heading - cameraCompassAngle
      # TODO Not sure, check
    } else {
      compassLog$cameraHeading <- compassLog$heading + cameraCompassAngle
    }

  } else if ( file.exists(analogCompassFile) ) {

    if (verbose) { disc_message("using analog compass") }

    stop("Not implemented yet")

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
  t$cameraHeading <- approx.circular(compassLog$dateTime, compassLog$cameraHeading, t$dateTime)$y

  # only keep larvae positions where a compass reading is available
  # might remove a few points at the beginning and end of the track, where the angle cannot be interpolated
  t <- t[ ! is.na(t$cameraHeading) , ]



  # Compute larvae tracks in a cardinal reference
  #--------------------------------------------------------------------------

  if (verbose) { disc_message("compute larvae tracks in a cardinal reference") }

  # read calibration data
  coordAquarium <- read.csv(make_path(dir, .files$aquarium.coord))

  # correct for the rotation, putting the north at upwards
  t <- ddply(t, ~trackNb, function(x) {

    # convert track to polar coordinates
    x[,c("theta","rho")] <- car2pol(x[,c("x","y")], c(coordAquarium$x, coordAquarium$y))

    # convert the angle to be measured as a bearing from the top of the frame
    x$theta <- as.bearing(x$theta)

    # reverse the direction of angles if we view the chamber from below
    if ( looking_up ) {
      x$theta <- from.below(x$theta)
    }

    # correct for the rotation: add the angle in the chamber relative to the top of the frame to the heading of the top of the frame to find the true heading
    xCor <- x
    xCor$theta <- as.bearing(x$theta + x$cameraHeading)

    # for un-rotated track to be comparable to the rotated one, they need to start in the same reference as the rotated track. So we use the *first* camera heading for every frame
    x$theta <- as.bearing(x$theta + x$cameraHeading[1])

    # label the rotated data
    xCor$rotation <- TRUE
    x$rotation <- FALSE
    # and combine the two
    x <- rbind(x, xCor)

    # recompute cartesian positions from the polar definition
    x[,c("x","y")] <- pol2car(x[,c("theta","rho")])

    # convert x, y, and rho to human significant measures (cm)
    # we use the diameter of the aquarium as a reference
    # we are given its value in cm and we have the perimeter, hence the diameter, in pixels
    px2cm <- diameter / ( coordAquarium$perim / pi )
    x[,c("x","y","rho")] <- x[,c("x","y","rho")] * px2cm

    return(x)
  })

# # TODO Compute swimming directions and speed when available
# ## Compute tracks characteristics   - BELOW HERE NEEDS WORK **** - MF
# #------------------------------------------------------------
#
# # Take omitted frames into account in larvae tracks
# # fetch the names of all images
# imagePath = paste("ls -1 ",dir,"/pics/*.jpg | cut -d '/' -f 3 | cut -d '.' -f 1", sep="")
# images = sort(as.numeric(system(imagePath, intern=T)))
# # images = sort(as.numeric(system("ls -1 ../pics/*.jpg | cut -d '/' -f 3 | cut -d '.' -f 1", intern=T)))
# # there are two levels of nesting of lists, hence the double llply construct
# tracks = llply(tracks, .fun=function(tr, ...){
#   llply(tr, .fun=function(x, imgNames) {
#     # prepare a full, empty data.frame with one line per image
#     t = as.data.frame(matrix(nrow=length(imgNames),ncol=length(names(x))))
#     names(t) = names(x)
#     # specify the content of columns that must not be empty
#     t$trackNb = x$trackNb[1]
#     t$correction = x$correction[1]
#     t$imgNb = imgNames
#     # set classes similarly to the original data.frame
#     class(t$date) = class(x$date)
#     class(t$exactDate) = class(x$exactDate)
#     # fill with values from the orignal data.frame and leave NAs elsewhere
#     t[ t$imgNb %in% x$imgNb,] = x;
#     return(t);}
#   , ...)}
# , images)
#
#
# # Compute swimming direction and speed
# for (i in 1:nbTracks) {
#   tracks[[i]] = llply(tracks[[i]], function(t) {
#     # Compute swimming directions
#     # compute swimming vectors in x and y directions
#     # = position at t+1 - position at t
#     dirs = t[2:nrow(t),c("x","y")] - t[1:(nrow(t)-1),c("x","y")]
#     dirs = rbind(NA,dirs)
#     # convert to headings by considering that these vectors originate from 0,0
#     headings = car2pol(dirs, c(0,0))$theta
#     # convert to the appropriate circular class
#     headings = conversion.circular(headings, units="degrees", template="geographics", modulo="2pi")
#     # store that in the orignal dataframe
#     t$heading = headings
#
#     # Compute speeds in cm/s
#     # compute time difference between pictures
#     dirs$interval = c(NA,as.numeric(diff(t$exactDate)))
#     # compute speed from displacement and interval
#     t$speed = sqrt(dirs$x^2 + dirs$y^2) / dirs$interval
#
#     return(t)
#   })
#
#   # Suppress speed for the uncorrected data: it does not make sense because the corrected "trajectory" is never really travelled
#   tracks[[i]][["corrected"]]$speed = NA
# }


  # Saving tracks for statistical analysis and plotting
  #--------------------------------------------------------------------------
  # Write it to a csv file
  destFile <- make_path(dir, .files$processed.tracks)
  if (verbose) { disc_message("write corrected track to ", destFile) }
  write.csv(t, file=destFile, row.names=F)

}
