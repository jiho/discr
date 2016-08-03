#' Raw data extraction
#'
#' Extract data corresponding to one deployment from the raw data recorded by a sensor over a leg.
#'
#' @param data data (or metadata) for this sensor, read by \code{\link{disc_read}}.
#' @param start,stop start and stop times, as \code{\link[base]{POSIXct}} objects.
#' @param dir destination directory, where the extracted data should be stored (without trailing slash, to be able to also name a file based on it).
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

#' @rdname disc_extract
#' @export
#' @param fps number of frames to extract per second. 0.5 gives one frame every two seconds.
#' @inheritParams disc_extract.gopro
disc_extract.goproVideo <- function(data, start, stop, dir, fps=1, width=1600, gray=FALSE, ...) {

  # create directory for videos
  dir.create(dir)
  
  # compute time interval for each video
  ds <- tidyr::spread(data, key=type, value=dateTime)
  ds <- dplyr::arrange(ds, begin)
  ds$interval <- lubridate::interval(ds$begin, ds$end)

  # detect which videos the deployment overlaps with
  deployInterval <- lubridate::interval(start, stop)
  ds <- ds[lubridate::int_overlaps(deployInterval, ds$interval),]
  show_nb_records(ds, dir)

  if (nrow(ds) >= 1) {
    files <- ds$file
    n <- length(files)
    ds$order <- 1:n

    # prepare temporary names for the video files we will cut from those original videos
    ds$tempfile <- make_path(dir, str_c("tempvid", ds$order, ".mp4"))
    
    # check time difference between videos if there are more than 1
    if (n > 1) {
      timeDifferences <- ds$end[1:(n-1)] - ds$begin[2:n]
      units(timeDifferences) <- "secs"
      timeDifferences <- as.numeric(timeDifferences)
      if (any(timeDifferences > 2) ) {
        stop("Gap of more than 2 seconds between videos that are supposed to be in the same deployment. This should not happen. Check your log.")
      }
    }
    
    # cut each video file (in parallel when there are more than 1)
    if (n>1) {
      doParallel::registerDoParallel(cores=min(n, parallel::detectCores()-1))
    }
    plyr::a_ply(ds, 1, function(x) {
      # define start time for ffmpeg
      if (start > x$begin) {
        startOffset <- start - x$begin
        # format it as an ffmpeg seek index HH:MM:SS.S
        startOffset <- format(ymd_hms("2000-01-01 00:00:00") + startOffset, "-ss %H:%M:%S.0")
      } else {
        startOffset <- NULL
      }
      
      # define end for ffmpeg
      if (stop < x$end) {
        stopOffset <- stop - x$begin
        stopOffset <- format(ymd_hms("2000-01-01 00:00:00") + stopOffset, "-to %H:%M:%S.0")
      } else {
        stopOffset <- NULL
      }
      
      # cut the original file
      exit <- system2("ffmpeg", str_c(startOffset, " -i ", x$file, " ", startOffset, " -c copy -an ", stopOffset, " ", x$tempfile), stdout=FALSE, stderr=FALSE)
      check_status(exit, str_c("Could not cut video file: ", x$file))
    }, .parallel=(n>1))


    # create final video
    # define a standard names the output file
    outputFile <- str_c(dir, ".mp4")
    # when there is only one video, just rename it
    if (n == 1) {
      file.rename(ds$tempfile, outputFile)
    # when there are several videos, join them
    } else {
      # prepare the file list for ffmpeg
      listFile <- make_path(dir, "list.txt")
      cat(str_c("file ", ds$tempfile), file=listFile, sep="\n")
      # concatenate files
      exit <- system2("ffmpeg", str_c("-f concat -safe 0 -i ", listFile, " -c copy -an ", outputFile), stdout=FALSE, stderr=FALSE)
      check_status(exit, str_c("Could not concatenate files: ", str_c(ds$tempfile, collapse=",")))
    }
    
    # cleanup and remove temporary files
    exit <- unlink(dir, recursive=TRUE)
    check_status(exit, str_c("Could not remove ", dir))
    
    # extract frames from the video
    # create the pics directory
    picsDir <- str_replace(dir, "vids", "pics")
    dir.create(picsDir)
    # extract frames
    exit <- system2("ffmpeg", str_c(" -i ", outputFile, " -vf fps=", fps, " ", picsDir, "/%d.jpg"), stdout=FALSE, stderr=FALSE)
    check_status(exit, "Could not extract frames from ", outputFile)
    # create the corresponding log file, with a timestamp for each picture
    picsFiles <- gtools::mixedsort(list.files(picsDir, full=TRUE))
    picsTimes <- start+(1:length(picsFiles)-1)*(1/fps)
    log <- data.frame(file=picsFiles, dateTime=picsTimes)
    show_nb_records(log, picsDir)
    logFile <- str_c(picsDir, "_log.csv")
    write.csv(log, file=logFile, row.names=FALSE)
  }

}
