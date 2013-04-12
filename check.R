#
#      Check that external software required by discuss are present
#      Check and set defaults for all options
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------

disc_check_option <- function(opts, token, default) {
  #
  # Check that an option exists. If it does not, provide a default
  #
  # opts    list of disc options
  # token   character string giving the option name, will be prefixed by "disc"
  # default default value to set if the options does not currently exist
  #

  # prefix the option name with "disc."
  suppressPackageStartupMessages(require("stringr", quietly=TRUE))
  token <- str_c("disc.", token)

  # check option and set default if necessary
  if (is.null(opts) | is.null(opts[[token]])) {
    opts[[token]] <- default
  }

  return(opts)
}

disc_check <- function() {
  #
  # Check that all required options have a value (and set a default if they don't)
  # In the meantime, check that the necessary software is installed
  #

  suppressPackageStartupMessages(require("stringr", quietly=TRUE))
  # read the current state of the options
  disc.opts <- disc_read_options()

  # options defaults
  disc.opts <- disc_check_option(disc.opts, "working_directory", make_path("."))
  disc.opts <- disc_check_option(disc.opts, "storage_directory", NA)
  disc.opts <- disc_check_option(disc.opts, "aquarium", "10,10,300,300")
  disc.opts <- disc_check_option(disc.opts, "camera_angle", 90)
  disc.opts <- disc_check_option(disc.opts, "diameter", 40)
  disc.opts <- disc_check_option(disc.opts, "java_memory", 1000)

  # check java
  disc.opts <- disc_check_option(disc.opts, "java",
                 check_software("java",
                   url="http://www.oracle.com/technetwork/java/javase/downloads/",
                   notes="You need a java JRE"
                 )
               )

  # check ImageJ
  disc.opts <- disc_check_option(disc.opts, "ij_path", make_path("./imagej/"))
  disc.opts <- disc_check_option(disc.opts, "ij_jar", make_path(disc.opts$disc.ij_path, "ij.jar"))
  if ( ! file.exists(disc.opts$disc.ij_jar) ) {
    warning("ImageJ not found, trying to download it")
    status <- download.file(url="http://rsb.info.nih.gov/ij/upgrade/ij.jar", destfile=disc.opts$disc.ij_jar, method="internal")
  }

  # check rsync
  disc.opts <- disc_check_option(disc.opts, "rsync", check_software("rsync"))

  # check exiftool
  disc.opts <- disc_check_option(disc.opts, "exif", check_software("exif"))

  # check R packages

  # write the (potentially modified) options file
  disc_write_options(disc.opts)

  return(invisible(disc.opts))
}
