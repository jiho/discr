#' Measure angle between camera and compass
#'
#' Measure angle of analog compasses on images, use that as a reference, compare it to the digital compass record and deduce the angle between the camera and the digital compass. This is mandatory to use the digital compass to correct the tracks.
#'
#' @param dir deployment directory
#'
#' @export
#'
#' @importFrom plyr ddply
#' @importFrom stringr str_c
disc_camera_compass_angle <- function(dir, ...) {

  picsDir <- make_path(dir, .files$pictures)
  if (! file.exists(picsDir)) {
    stop("Cannot find directory ", picsDir)
  }

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"))
  if (length(pics) == 0) {
    stop("Cannot find pictures in ", picsDir)
  }

  # open every n images in the folder and manually measure the compass angle on each
  # save the results to a temporary file
  n <- 50
  compassAngleFile <- tempfile()

	message("COMPASS ANGLE")
	message("Opening images for compass angle detection")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discuss"),
    " -ijpath ", system.file("ij/", package="discuss"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=-1 starting=1 increment=", n, " scale=100 file=[] or=[] sort');",
    # " run('Measure Angle', '');",
    " run('Compile and Run...', 'compile=", system.file("ij/", package="discuss"),"/plugins/Measure_Angle.java');",
    " waitForUser('Compass detect',",
    " 'Draw a line from the South to the North of a compass.\\nClick Get Angle. Repeat for every compass.\\nMove to the next image until the end of the stack.\\nPress OK when you are done');",
    " saveAs('Measurements', '", compassAngleFile, "');",
    " run('Quit');\""
  )

  # run the command and check for success
  status <- system(command)
  check_status(status, message="Abort compass angle measure")

  # save aquarium center and perimenter
  if (file.exists(compassAngleFile)) {

    # read the file with compass angle measurements
    d <- read.delim(compassAngleFile, stringsAsFactors=FALSE)

    # compute a mean heading from the measurements of the compass angles, in each image
    d$Angle <- as.angle(d$Angle)
    d$Heading <- as.heading(d$Angle)
    measuredHeadings <- ddply(d, ~Slice, function(x) {mean(x$Heading)})

    # get the capture time of the images on which the angle was measured
    # compute all images that were displayed
    imgsNumbers <- seq(from=1, to=length(pics), by=n)
    # restrict to those on which the heading was measured
    imgsNumbers <- imgsNumbers[measuredHeadings$Slice]
    # get the corresponding times
    imgs <- read.csv(make_path(dir, str_c(.files$pictures, ".csv")), stringsAsFactors=FALSE)
    imgs <- imgs[imgsNumbers,]
    imgsTimes <- as.POSIXct(imgs$dateTime)

    # read the compass records
    compass <- read.csv(make_path(dir, .files$digital.compass), stringsAsFactors=FALSE)
    compass$dateTime <- as.POSIXct(compass$dateTime)
    compass$heading <- as.heading(compass$heading)

    # get the compass readings for the images on which the angle was measured
    recordedHeadings <- approx.circular(x=compass$dateTime, angles=compass$heading, xout=imgsTimes)$y

    # compute the mean difference angle
    cameraCompassAngles <- measuredHeadings$V1 - recordedHeadings
    cameraCompassAngle <- mean(cameraCompassAngles)

    # display it and store it in a file
    message("Camera compass angle is: ", round(cameraCompassAngle, 2) )
    dput(cameraCompassAngle, file=make_path(dir, .files$camera.compass.angle))

  } else {
    stop("Abort compass angle measure")
  }

  return(invisible(status))

}
