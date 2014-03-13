#' Calibrate aquarium position
#'
#' @param dir path the to the deployment directory
#'
#' @export
#' @importFrom stringr str_c
disc_calibrate <- function(dir, ...) {

  picsDir <- make_path(dir, .files$pictures)
  if (! file.exists(picsDir)) {
    stop("Cannot find directory ", picsDir)
  }

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"))
  if (length(pics) == 0) {
    stop("Cannot find pictures in ", picsDir)
  }

  # prepare storage
  aquariumCoordFile <- tempfile()
  aquariumBoundingBoxFile <- tempfile()

	message("CALIBRATION")
	message("Opening first image for calibration")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("inst/ij/ij.jar", package="discuss"),
    " -ijpath ", system.file("inst/ij/", package="discuss"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=1 starting=1 increment=1 scale=100 file=[] or=[] sort');",
    " makeOval(", getOption("disc.aquarium"), ");",
    " setTool('oval');",
    " waitForUser('Aquarium selection',",
    " 'If necessary, alter the selection to fit the aquarium better.\\n",
    " Press OK when you are done');",
    " run('Set Measurements...', '  centroid perimeter invert redirect=None decimal=3');",
    " run('Measure');",
    " saveAs('Measurements', '", aquariumCoordFile, "');",
    " run('Clear Results');",
    " run('Set Measurements...', '  bounding redirect=None decimal=3');",
    " run('Measure');",
    " saveAs('Measurements', '", aquariumBoundingBoxFile, "');",
    " run('Quit');\""
  )

  # run the command and check for success
  status <- system(command)
  check_status(status, message="Abort calibration")

  # save aquarium center and perimenter
  if (file.exists(aquariumCoordFile)) {
    file.copy(aquariumCoordFile, make_path(dir, .files$aquarium.coord))
    file.remove(aquariumCoordFile)
  } else {
    stop("Abort calibration")
  }

  # save bouding box of aquarium in setting
  if (file.exists(aquariumBoundingBoxFile)) {
    bounds <- read.delim(aquariumBoundingBoxFile, row.names=1)
    bounds <- str_c(as.character(bounds[1,]), collapse=",")
    disc_conf(aquarium=bounds)
    file.remove(aquariumBoundingBoxFile)
  } else {
    stop("Abort calibration")
  }

  return(invisible(status))
}

#' @rdname disc_calibrate
#' @export
dcalibrate <- disc_calibrate

#' @rdname disc_calibrate
#' @export
dcalib <- disc_calibrate