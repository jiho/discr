disc_track <- function(deployment, sub=NULL, verbose=FALSE, ...) {
  #
  # Use the manual tracking plugin to record a track
  #
  # deployment  deployment number
  # sub         subsampling interval, in s
  # verbose     output messages on the console when TRUE

  suppressPackageStartupMessages(require("stringr", quietly=TRUE))
  disc.opts <- disc_read_options()

  # setup workspace and output files
  deploy <- make_path(disc.opts$disc.working_directory, deployment)
  trackFile <- make_path(deploy, "larvae_track.txt")
  pics <- make_path(deploy, "pics", "*.jpg")

  # check that we have images
  allPics <- Sys.glob(pics)
  if (length(allPics) == 0) {
    stop("Pictures not found in ", pics)
  }

  # Determine sub-sampling rate, if any
  # compute interval between images
  interval <- as.numeric(mean(diff(image_time(allPics))))
  # compute the subsampling rate
  if ( is.null(sub) ) {
    subN <- 1
  } else {
    subN <- round(sub / interval)
    # one image every subN will give an interval of sub seconds, approximately
    if (verbose) {
      message("subsampling at ", round(subN * interval, 2), " seconds, on average")
    }
  }

	# Determine whether to use a virtual stack or a real one
	# nb of images opened = total / subsampling rate
	nbOpened <- length(allPics) / subN
	# when there are less than 30 frames to open, loading them is fast and not too memory hungry
	# in that case, use a regular stack, other wise use a virtual stack
	if ( nbOpened <= 30 ) {
	  virtualStack <- ""
	} else {
	  virtualStack <- "use"
	}

	if (verbose) message("Open stack")
	# Use an ImageJ macro to run everything. The macro proceeds this way
	# - use Image Sequence to open the stack
	# - call the Manual Tracking plugin
	# - use waitForUser to let the time for the user to track larvae
	# - save the tracks to an appropriate file
	# - quit
  command <- str_c(
    disc.opts$disc.java, " -Xmx", disc.opts$disc.java_memory, "m -jar ", disc.opts$disc.ij_jar,
    " -ijpath ", disc.opts$disc.ij_path, " -eval \"",
    " run('Image Sequence...', 'open=", pics, " number=0 starting=1 increment=", subN, " scale=100 file=[] or=[] sort ", virtualStack,"');",
    " run('Manual Tracking');",
    " waitForUser('Track finished?',",
    " 'Press OK when done tracking');",
    " selectWindow('Tracks');",
    " saveAs('Text', '",trackFile,"');",
    " run('Quit');\""
  )
  if (verbose) message("Running:", command)

  status <- system(command)
  check_status(status, message="Could not perform manual tracking")

  return(invisible(status))
}