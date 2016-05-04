#' Measure angle between camera and compass
#'
#' Measure angle of analog compasses on images, use that as a reference, compare it to the digital compass record and deduce the angle between the camera and the digital compass. This is mandatory to use the digital compass to correct the tracks.
#'
#' @param dir deployment directory
#' @param sub subsampling interval, in s
#' @param verbose output messages on the console when TRUE
#' @param ... passthrough argument
#'
#' @export
#' @family action functions
#'
#' @importFrom plyr ddply
#' @importFrom stringr str_c
#' @importFrom assertthat assert_that
#' @importFrom circular mean.circular angular.deviation
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discusr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#' deploy1 <- paste0(dd, "1")
#'
#' # run the action
#' disc_conf(deploy.dir=dd)
#' \donttest{disc_camera_compass_angle(dir=deploy1, sub=10, verbose=TRUE)}
#' # the angle is also stored in a file "angle_camera_compass.txt"
#' list.files(deploy1)
disc_camera_compass_angle <- function(dir, sub=NULL, verbose=FALSE, ...) {

  disc_message("Camera / compass angle")

  # checks
  picsDir <- make_path(dir, .files$pictures)
  assert_that(file.exists(picsDir))

  picsFile <- make_path(dir, str_c(.files$pictures, "_log.csv"))
  assert_that(file.exists(picsFile))

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"))
  assert_that(not_empty(pics))

  # Determine sub-sampling rate, if any
  picsData <- read.csv(picsFile)
  subN <- subsample_n(ymd_hms(picsData$dateTime), sub=sub, verbose=verbose)

  # open every subN images in the folder and manually measure the compass angle on each
  # save the results to a temporary file
  compassAngleFile <- tempfile()

	if ( verbose ) disc_message("opening images for compass angle detection")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discusr"),
    " -ijpath ", system.file("ij/", package="discusr"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=-1 starting=1 increment=", subN, " scale=100 file=[] or=[] sort');",
    " run('Measure Angle', '');",
    # " run('Compile and Run...', 'compile=", system.file("ij/", package="discusr"),"/plugins/Measure_Angle.java');",
    " waitForUser('Compass detect',",
    " 'Draw a line from the South to the North of a compass.\\nClick Get Angle. Repeat for every compass.\\nMove to the next image until the end of the stack.\\nPress OK when you are done');",
    " saveAs('Measurements', '", compassAngleFile, "');",
    " run('Quit');\""
  )

  # run the command and check for success
  status <- system(command)
  check_status(status, message="Abort compass angle measure")

  assert_that(file.exists(compassAngleFile))
  if ( verbose ) disc_message("reading records in ", compassAngleFile)

  # read the file with compass angle measurements
  d <- read.delim(compassAngleFile, stringsAsFactors=FALSE)

  # compute the heading of the top of the image (direction towards which it points, computed from the North, clockwise, in degrees)

  # angle of the north of the compass in degrees from the horizontal
  d$angleOfNorthOfCompass <- as.angle(d$Angle)
  # convert into a "bearing" = angle clockwise from the top of the image to the north of the compass
  # numerically, this is equivalent to the bearing of the top of the camera from the North (i.e. the heading of the camera), because we look at the compass from below, hence this heading is measured counter clockwise on the image
  d$cameraHeading <- as.bearing(d$angleOfNorthOfCompass)
  # compute the mean heading for every frame
  cameraMeanHeadings <- ddply(d, ~Slice, function(x) {mean(x$cameraHeading)})

  # get the capture time of the images on which the angle was measured
  # compute all images that were displayed
  imgsNumbers <- seq(from=1, to=length(pics), by=subN)
  # restrict to those on which the heading was measured
  imgsNumbers <- imgsNumbers[cameraMeanHeadings$Slice]
  # get the corresponding times
  imgs <- read.csv(make_path(dir, str_c(.files$pictures, "_log.csv")), stringsAsFactors=FALSE)
  imgs <- imgs[imgs$imgNb %in% imgsNumbers,]
  imgsTimes <- as.POSIXct(imgs$dateTime)

  # read the compass records
  compass <- read.csv(make_path(dir, .files$digital.compass), stringsAsFactors=FALSE)
  compass$dateTime <- as.POSIXct(compass$dateTime)
  compass$heading <- as.bearing(compass$heading)

  # get the compass readings for the images on which the angle was measured
  compassHeadings <- approx_circular(x=compass$dateTime, angles=compass$heading, xout=imgsTimes)$y

  # compute the mean difference angle
  cameraCompassAngles <- compassHeadings - cameraMeanHeadings$V1
  cameraCompassAngle  <- mean.circular(cameraCompassAngles)
  sdCompassAngle      <- angular.deviation(cameraCompassAngles) * 180 / pi
  # in the following the position of the larva will be recorded relative to the top of the frame of the camera. This cameraCompassAngle will need to be *subtracted* from the angle from the top of the camera to find the true heading of the larva

  # display it and store it in a file
  message("Camera compass angle is: ", round(cameraCompassAngle, 1), "+/-", round(sdCompassAngle, 1) )
  dput(as.numeric(cameraCompassAngle), file=make_path(dir, .files$camera.compass.angle))

  return(invisible(status))
}
