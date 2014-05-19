#' Manually track a larva
#'
#' Use the manual tracking plugin to record a track
#'
#' @param dir deployment directory
#' @param sub subsampling interval, in s
#' @param verbose output messages on the console when TRUE
#'
#' @export
disc_track <- function(dir, sub=NULL, verbose=FALSE, ...) {

  picsDir <- make_path(dir, .files$pictures)
  if (! file.exists(picsDir)) {
    stop("Cannot find directory ", picsDir)
  }

  pics <- list.files(picsDir, pattern=glob2rx("*.jpg"), full=TRUE)
  if (length(pics) == 0) {
    stop("Cannot find pictures in ", picsDir)
  }


  # Determine sub-sampling rate, if any
  # compute interval between images
  interval <- mean(as.numeric(diff(image_time(na.omit(pics[1:30])))))
  # TODO check how relevant it is to use only 30 images
  # TODO when have image_interval for this...
  # compute the subsampling rate
  if ( is.null(sub) ) {
    subN <- 1
  } else {
    subN <- round(sub / interval)
    # one image every subN will give an interval of sub seconds, approximately
    if (verbose) {
      dmessage("subsampling at ", round(subN * interval, 2), " seconds, on average")
    }
  }

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

	if (verbose) dmessage("Open stack")
	# Use an ImageJ macro to run everything. The macro proceeds this way
	# - use Image Sequence to open the stack
	# - call the Manual Tracking plugin
	# - use waitForUser to let the time for the user to track larvae
	# - save the tracks to an appropriate file
	# - quit
  command <- str_c(
    "java -Xmx", getOption("disc.java_memory"), "m -jar ", system.file("ij/ij.jar", package="discuss"),
    " -ijpath ", system.file("ij/", package="discuss"), " -eval \"",
    " run('Image Sequence...', 'open=", picsDir, " number=0 starting=1 increment=", subN, " scale=100 file=[] or=[] sort ", virtualStack,"');",
    " run('Compile and Run...', 'compile=", system.file("ij/", package="discuss"),"/plugins/Manual_Tracking.java');",
    # " run('Manual Tracking');",
    # TODO investigate wether compilating on the fly may be a problem
    " waitForUser('Track finished?',",
    " 'Press OK when done tracking');",
    " selectWindow('Tracks');",
    " saveAs('Text', '", make_path(dir, .files$tracks), "');",
    " run('Quit');\""
  )
  if (verbose) dmessage("Running:", command)

  status <- system(command)
  check_status(status, message="Could not perform manual tracking")

  return(invisible(status))
}