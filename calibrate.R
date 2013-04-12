disc_calibrate <- function(deployment, ...) {

  suppressPackageStartupMessages(require("stringr", quietly=TRUE))

  # checks
  disc.opts <- disc_read_options()
  
  deploy <- make_path(disc.opts$disc.working_directory, deployment)
  
  pics <- make_path(deploy, "pics", "*.jpg")
  if (length(Sys.glob(pics)) == 0) {
    stop("Pictures not found in ", pics)
  }
  
  aquariumCoordFile <- make_path(deploy, "coord_aquarium.txt")
  aquariumBoundingBoxFile <- tempfile()

	message("\nCALIBRATION\n")
	message("Open first image for calibration")

  # prepare java command
  command <- str_c(
    disc.opts$disc.java, " -Xmx200m -jar ", disc.opts$disc.ij_jar,
    " -ijpath ", disc.opts$disc.ij_path, " -eval \"",
    " run('Image Sequence...', 'open=", pics, " number=1 starting=1 increment=1 scale=100 file=[] or=[] sort');",
    " makeOval(", disc.opts$disc.aquarium, ");",
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
  check_status(status, message="Could not calibrate aquarium size")

  # save bouding box of aquarium in options
  bounds <- read.delim(aquariumBoundingBoxFile, row.names=1)
  disc.opts$disc.aquarium <- str_c(as.character(bounds[1,]), collapse=",")
  # remove tempory file
  file.remove(aquariumBoundingBoxFile)
  
  # save options
  disc_write_options(disc.opts)
  
  return(invisible(status))
}