#' Manually track a larva
#'
#' Use the manual tracking plugin to record a track
#'
#' @param dir deployment directory
#' @param sub subsampling interval, in s
#' @param verbose output messages on the console when TRUE
#' @param ... passthrough argument
#'
#' @export
#' @family action functions
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#' deploy1 <- paste0(dd, "1")
#'
#' # run the action
#' disc_conf(deploy.dir=dd)
#' \donttest{disc_track(dir=deploy1, verbose=TRUE)}
#' # inspect results
#' list.files(deploy1)
#' head(read.csv(paste0(deploy1, "/larvae_tracks.csv")))
disc_track <- function(dir, sub=NULL, verbose=FALSE, ...) {

  disc_message("Track")

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

	# Determine whether to use a virtual stack or a real one
	# nb of images opened = total / subsampling rate
	nbOpened <- length(pics) / subN
	# when there are less than 30 frames to open, loading them is fast and not too memory hungry
	# in that case, use a regular stack, other wise use a virtual stack
	if ( nbOpened <= 30 ) {
	  virtualStack <- ""
	} else {
	  virtualStack <- "use"
	}

	if (verbose) disc_message("open stack for tracking")
  # prepare temporary storage
  larvaTracksFile <- tempfile(fileext=".txt")

	# Use an ImageJ macro to run everything. The macro proceeds this way
	# - use Image Sequence to open the stack
	# - call the Manual Tracking plugin
	# - use waitForUser to let the time for the user to track larvae
	# - save the tracks to an appropriate file
	# - quit
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discr"),
    " -ijpath ", system.file("ij/", package="discr"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=0 starting=1 increment=", subN, " scale=100 file=[] or=[] sort ", virtualStack,"');",
    " run('Manual Tracking', '');",
    # " run('Compile and Run...', 'compile=", system.file("ij/", package="discr"),"/plugins/Manual_Tracking.java');",
    " waitForUser('Track finished?',",
    " 'Press OK when done tracking');",
    " selectWindow('Tracks');",
    " saveAs('Text', '", larvaTracksFile, "');",
    " run('Quit');\""
  )
  status <- system(command)
  check_status(status, message=str_c("Error running command\n:", command, "\nAbort"))

  # save larva track
  assert_that(file.exists(larvaTracksFile))
  destFile <- make_path(dir, .files$tracks)
  if ( verbose ) disc_message("write track to ", destFile)
  d <- read.delim(larvaTracksFile, row.names=1)
  write.csv(d, file=destFile, row.names=FALSE)
  file.remove(larvaTracksFile)

  return(invisible(status))
}
