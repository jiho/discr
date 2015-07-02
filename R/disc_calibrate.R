#' Calibrate aquarium position
#'
#' @param dir path to the deployment directory
#' @param verbose output messages on the console when TRUE
#' @param ... passthrough argument
#'
#' @export
#' @family action functions
#'
#' @importFrom stringr str_c
#' @importFrom assertthat assert_that not_empty
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#' deploy1 <- paste0(dd, "1")  # digital compass
#'
#' # run the action
#' disc_conf(deploy.dir=dd)
#' \donttest{disc_calibrate(dir=deploy1, verbose=TRUE)}
#' # inspect results
#' list.files(deploy1)
#' # note that the aquarium coordinates are remembered
#' disc_conf(deploy.dir=dd)
disc_calibrate <- function(dir, verbose=FALSE, ...) {

  disc_message("Calibrate")

  # checks
  picsDir <- make_path(dir, .files$pictures)
  assert_that(file.exists(picsDir))

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"))
  assert_that(not_empty(pics))


  # prepare storage
  aquariumCoordFile <- tempfile()
  aquariumBoundingBoxFile <- tempfile()

  if ( verbose ) disc_message("open first image for calibration")

  # prepare java command
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discr"),
    " -ijpath ", system.file("ij/", package="discr"), " -eval \"",
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
  check_status(status, message=str_c("Error running command\n:", command, "\nAbort"))

  # save aquarium centre and perimeter
  assert_that(file.exists(aquariumCoordFile))
  destFile <- make_path(dir, .files$aquarium.coord)
  if ( verbose ) disc_message("write aquarium coordinates to ", destFile)
  d <- read.delim(aquariumCoordFile, row.names=1, col.names=c("nb", "x", "y", "perim"))
  write.csv(d, file=destFile, row.names=FALSE)
  file.remove(aquariumCoordFile)

  # save bounding box of aquarium in settings
  assert_that(file.exists(aquariumBoundingBoxFile))
  if ( verbose ) disc_message("cache aquarium bounding box in conf file")
  bounds <- read.delim(aquariumBoundingBoxFile, row.names=1)
  bounds <- str_c(as.character(bounds[1,]), collapse=",")
  disc_conf(aquarium=bounds)
  file.remove(aquariumBoundingBoxFile)

  return(invisible(status))
}
