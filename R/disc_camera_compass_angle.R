#' Measure the angle between the camera and the compass
#'
#' @param dir deployment directory
#'
#' @export
#'
#' @import circular
disc_camera_compass_angle <- function(dir, ...) {

  picsDir <- make_path(dir, .files$pictures)
  if (! file.exists(picsDir)) {
    stop("Cannot find directory ", picsDir)
  }

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"))
  if (length(pics) == 0) {
    stop("Cannot find pictures in ", picsDir)
  }

  # prepare storage
  compassAngleFile <- tempfile()

	message("COMPASS ANGLE")
	message("Opening first image for compass angle detection")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discuss"),
    " -ijpath ", system.file("ij/", package="discuss"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=-1 starting=1 increment=50 scale=100 file=[] or=[] sort');",
    " setTool('line');",
    " waitForUser('Compass detect',",
    " 'Draw a line from the South to the North of a compass.\\nMeasure it with Analyze > Measure.\\nRepeat for every compass and\\nseveral times per compass if time permits.\\n\\nPress OK when you are done');",
    " saveAs('Measurements', '", compassAngleFile, "');",
    " run('Quit');\""
  )
  # TODO record slice number

  # run the command and check for success
  status <- system(command)
  check_status(status, message="Abort compass angle measure")

  # save aquarium center and perimenter
  if (file.exists(compassAngleFile)) {
    
    # read the file with compass angle measurements
    d <- read.delim(compassAngleFile, stringsAsFactors=FALSE)
    
    # compute the angle as a mean heading of the various measurements of the compass
    d$Angle <- as.angle(d$Angle)
    d$Angle <- as.heading(d$Angle)
    obsHeading <- mean(d$Angle)

    # get the corresponding measurement from the digital compass
    # get the time of the image
    img <- read.csv(make_path(dir, str_c(.files$pictures, ".csv")), stringsAsFactors=FALSE)
    imageTime <- as.POSIXct(img$dateTime[1])

    # get the corresponding compass value
    compass <- read.csv(make_path(dir, .files$digital.compass), stringsAsFactors=FALSE)
    compass$dateTime <- as.POSIXct(compass$dateTime)
    compass$heading <- as.heading(compass$heading)
    compassHeading <- approx.circular(x=compass$dateTime, angles=compass$heading, xout=imageTime)$y
    
    cameraCompassAngle <- obsHeading - compassHeading
        
    message("Camera compass angle is: ", round(cameraCompassAngle, 2) )
  } else {
    stop("Abort compass angle measure")
  }

  return(invisible(status))
  
}
