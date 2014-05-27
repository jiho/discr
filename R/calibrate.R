#' Calibrate aquarium position
#'
#' @param dir path to the deployment directory
#' @param ... passthrough argument
#'
#' @export
#' @importFrom stringr str_c
disc_calibrate <- function(dir, ...) {

	disc_message("calibration")

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

  # message("Opening first image for calibration")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discuss"),
    " -ijpath ", system.file("ij/", package="discuss"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=1 starting=1 increment=1 scale=100 file=[] or=[] sort');",
    " makeOval(", getOption("disc.aquarium"), ");",
    " setTool('oval');",
    " waitForUser('Aquarium selection',",
    " 'If necessary, alter the selection to fit the aquarium better.\\nPress OK when you are done');",
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
  check_status(status, message="Error running ImageJ. Aborting")

  # save aquarium centre and perimentre
  if (file.exists(aquariumCoordFile)) {
    d <- read.delim(aquariumCoordFile, row.names=1, col.names=c("nb", "x", "y", "perim"))
    write.csv(d, file=make_path(dir, .files$aquarium.coord), row.names=FALSE)
    file.remove(aquariumCoordFile)
  } else {
    stop("Cannot find aquarium coordinates file: ", aquariumCoordFile, ". Aborting")
  }

  # save bounding box of aquarium in settings
  if (file.exists(aquariumBoundingBoxFile)) {
    bounds <- read.delim(aquariumBoundingBoxFile, row.names=1)
    bounds <- str_c(as.character(bounds[1,]), collapse=",")
    disc_conf(aquarium=bounds)
    file.remove(aquariumBoundingBoxFile)
  } else {
    stop("Cannot find aquarium bounding box file: ", aquariumBoundingBoxFile, ". Aborting")
  }

  return(invisible(status))
}
