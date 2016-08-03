#' Raw data extraction
#'
#' Extract data corresponding to one deployment from the raw data recorded by a sensor over a leg.
#'
#' @param data data (or metadata) for this sensor, read by \code{\link{disc_read}}.
#' @param start,stop start and stop times, as \code{\link[base]{POSIXct}} objects.
#' @param dir destination directory, where the extracted data should be stored.
#' @param ... passed to the various methods.
#'
#' @details
#' All methods can rely on \code{data} having a \code{dateTime} column, of class \code{\link[base]{POSIXct}}. Based on this column, they should extract the data between \code{start} and \code{stop} and store it in the destination directory.
#'
#' @seealso \code{\link{disc_read}} and \code{\link{disc_extract_deployments}}
#'
#' @export
disc_extract <- function(data, start, stop, dir, ...) {
  UseMethod("disc_extract")
}

# Utility function to display the number of records (lines) in a data.frame
show_nb_records <- function(x, dir) {
  # count the number of records
  n <- nrow(x)
  # and give a nice summary message
  disc_message(format(basename(dir), width=10), " ", format(n, width=4), if(n==0) { " !!!" })
}

#' @rdname disc_extract
#' @export
disc_extract.default <- function(data, start, stop, dir, ...) {
  # get the corresponding data
  ds <- dplyr::filter(data, dateTime >= start, dateTime <= stop)
  show_nb_records(ds, dir)

  # if there are enough, write the selected portion of the data to the deployment folder
  if (nrow(ds) > 1) {
    write.csv(ds, file=str_c(dir, "_log.csv"), row.names=FALSE)
  }
  
}

#' @rdname disc_extract
#' @export
#' @param width width to resize the images to, in pixels. When NULL, images are not resized.
#' @param gray logical; whether to convert the images to grayscale.
disc_extract.gopro <- function(data, start, stop, dir, width=1600, gray=FALSE, ...) {
  # get the corresponding data
  ds <- dplyr::filter(data, dateTime >= start, dateTime <= stop)
  
  # count the number of data points
  show_nb_records(ds, dir)

  # number pictures sequentially
  ds$imgNb <- 1:nrow(ds)
  ds$file <- make_path(dir, str_c(ds$imgNb, ".jpg"))

  # if there are enough
  if (nrow(ds) > 1) {
    # write the selected portion of the data to the deployment folder
    write.csv(ds, file=str_c(dir, "_log.csv"), row.names=FALSE)

    # create the pictures directory for this deployment
    dir.create(dir, showWarnings=FALSE)

    # copy or convert the original images into their destination directory
    doParallel::registerDoParallel(parallel::detectCores()-1)
    a_ply(ds, 1, function(x) {
      if ( is.null(width) & ! gray ) {
        file.copy(x$origFile, x$file)
      } else {
        if (!is.null(width)) {
          width <- str_c("-resize ", width,"x")
        }
        if (gray) {
          gray <- "-colorspace gray"
        } else {
          gray <- NULL
        }
        system(str_c("convert \"", x$origFile, "\" ", width, " ", gray, " \"", x$file, "\""))
      }
    }, .parallel=FALSE)
  }

}

