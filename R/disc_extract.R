#' Raw data extraction
#'
#' Extract data corresponding to one deployment from the raw data recorded by a sensor over a leg.
#'
#' @param data data (or metadata) for this sensor, read by \code{\link{disc_read}}.
#' @param start,stop start and stop times, as \code{\link[base]{POSIXct}} objects.
#' @param dir destination directory, where the extracted data should be stored (without trailing slash, to be able to also name a file based on it).
#' @param verbose output additional information (for debugging purposes) when extracting data.
#' @param ... passed to the various methods.
#'
#' @details
#' All methods can rely on \code{data} having a \code{dateTime} column, of class \code{\link[base]{POSIXct}}. Based on this column, they should extract the data between \code{start} and \code{stop} and store it in the destination directory.
#'
#' @seealso \code{\link{disc_read}} and \code{\link{disc_extract_deployments}}
#'
#' @export
disc_extract <- function(data, start, stop, dir, verbose, ...) {
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
disc_extract.default <- function(data, start, stop, dir, verbose=FALSE, ...) {
  # get the corresponding data
  ds <- dplyr::filter(data, dateTime >= start, dateTime <= stop)
  show_nb_records(ds, dir)

  # if there are enough, write the selected portion of the data to the deployment folder
  if (nrow(ds) > 1) {
    write.csv(ds, file=str_c(dir, "_log.csv"), row.names=FALSE)
    if (verbose) {
      print(head(ds))
    }
  }
  
}


# Resize images and convert them to grayscale
process_images <- function(files, width, gray, parallel) {
  # convert function arguments into convert/mogrify options
  if (!is.null(width)) {
    width <- str_c("-resize ", width, "x")
  }
  
  if (gray) {
    gray <- "-colorspace gray"
  } else {
    gray <- NULL
  }

  # if nothing is to be done, exit immediately
  if (is.null(with) & is.null(gray)) {
    return(invisible(NULL))
  }
  # other wise process images (in parallel)
  if (parallel) doParallel::registerDoParallel(parallel::detectCores()-1)
  plyr::l_ply(files, function(x) {
    system(str_c("mogrify ", width, " ", gray, " -quality 100 \"", x, "\""))
  }, .parallel=parallel)
  if (parallel) doParallel::stopImplicitCluster()
}

# Difftime in seconds
diff_secs <- function(time1, time2) {
  as.numeric(difftime(time1, time2, units="secs"))
}

#' @rdname disc_extract
#' @export
#' @param width width to resize the images to, in pixels. When NULL, images are not resized.
#' @param gray logical; whether to convert the images to grayscale.
disc_extract.gopro <- function(data, start, stop, dir, verbose=FALSE, width=1600, gray=FALSE, parallel=TRUE, ...) {
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
    dir.create(dir)

    # copy the original images into their destination directory
    if (verbose) dmessage("copying images")
    exit <- file.copy(ds$origFile, ds$file)
    check_status(any(!exit), str_c(sum(!exit), " frames could not be copied to ", dir))
    # and process them if needed
    if (verbose) dmessage("processing images")
    process_images(ds$file, width=width, gray=gray, parallel=parallel)
  }
}

#' @rdname disc_extract
#' @export
#' @param fps number of frames to extract per second. 0.5 gives one frame every two seconds, 1/3 gives one frame every 3 seconds, 2 gives one frame every 0.5 seconds.
#' @param parallel logical, whether to process files in parallel (should be left to TRUE by default for speed).
#' @inheritParams disc_extract.gopro
disc_extract.goproVideo <- function(data, start, stop, dir, verbose=FALSE, fps=1, width=1600, gray=FALSE, parallel=TRUE, ...) {
  
  # compute time interval for each source video
  # NB: GoPros cut files in ~ 21 mins portions
  d <- tidyr::spread(data, key="type", value="dateTime")
  d <- dplyr::arrange(d, begin)
  d$interval <- lubridate::interval(d$begin, d$end)

  # detect which source videos the deployment overlaps with
  deployInterval <- lubridate::interval(start, stop)
  ds <- d[lubridate::int_overlaps(deployInterval, d$interval),]
  show_nb_records(ds, dir)
  n <- nrow(ds)

  # from each source video, we:
  # - extract the piece we need
  # - extract pictures
  # NB: 
  # to extract pictures, we start from the source video again which is the best way to make the first frames match between the extracted deployment video and the still frames
  # when the deployment overlaps two source videos, we do not concatenate the source videos then extract the full deployment because that (i) is longer, (ii) creates a large, useless, temporary file, (iii) looses track of the start time of the video
  # => we treat each source video independently
  if (n < 1) {
    stop("No video matches this deployment. Check your deployment log.")
  } else {
    # check time difference between source videos if there are more than 1
    if (n > 1) {
      timeDifferences <- diff_secs(ds$begin[2:n], ds$end[1:(n-1)])
      # detect jumps
      if (any(timeDifferences > 2) ) {
        stop("Gap of more than 2 seconds between source videos that are supposed to span the same deployment. This should not happen. Check your deployments log.")
      }
    }
    
    # create directory for output videos
    dir.create(dir)
    # and temporary storage
    temp <- make_path(dir, "temp")
    dir.create(temp)
    # TODO this could just be dir
    
    # record the order of files arranged by begin time
    # (used later on to define temporary names etc.)
    ds$order <- 1:n
    # prepare temporary file name(s) for the file we will cut from the source video
    ds$temp_video_file <- make_path(temp, str_c("vid", ds$order, ".mp4"))
    # prepare temporary directory(ies) for the frames we will extract from the source video
    ds$temp_pics_dir   <- make_path(temp, str_c("pics", ds$order))
    if (verbose) print(ds)
    
    # process each source video file (in parallel when there are more than 1)
    # = cut the video
    #   extract frames, compute time stamps and return the data
    if ( parallel & n > 1) {
      doParallel::registerDoParallel(cores=min(n, parallel::detectCores()-1))
    }
    pics <- plyr::adply(ds, 1, function(x) {
      ## Cut source video
      if (verbose) dmessage("cut video: ", x$file)
      
      # start of the deployment from the start of the current video
      if (start > x$begin) {
        start_offset_num <- diff_secs(start, x$begin)
        start_offset <- str_c("-ss ", start_offset_num)
      } else {
        start_offset_num <- 0
        start_offset <- NULL
      }
      
      # end of the deployment from the *start* of the current video
      # (i.e. duration of video to extract)
      if (stop < x$end) {
        stop_offset <- str_c("-t ", diff_secs(stop, x$begin))
      } else {
        stop_offset <- NULL
      }
      
      # cut the source video
      exit <- system2("ffmpeg", str_c(" -accurate_seek ", start_offset, " -i \"", x$file, "\" ", stop_offset, " -c copy -an \"", x$temp_video_file, "\""), stdout=FALSE, stderr=ifelse(verbose, "", FALSE))
      check_status(exit, str_c("Could not cut video file: ", x$file))
      
      ## Extract frames
      if (verbose) dmessage("extract images from: ", x$file)
      
      # let us assume fps = 1/3 (one frame every 3 seconds). The frame for the first 3s will be in the middle of the segment, at 1.5s, while we would like it to match the video we just cut above, and compass record, etc. and be at 0s. So we start the video early here, by half a segment ((1/fps)/2), to account for it.
      if (start > x$begin) {
        start_offset <- str_c("-ss ", start_offset_num - ((1/fps)/2))
      }

      # create the temporary directory
      dir.create(x$temp_pics_dir)

      # extract frames from the source video
      exit <- system2("ffmpeg", str_c(" -accurate_seek ", start_offset, " -i \"", x$file, "\" ", stop_offset, " -qscale:v 2 -vf fps=", fps, " \"", x$temp_pics_dir, "/%05d.jpg\""), stdout=FALSE, stderr=ifelse(verbose, "", FALSE))
      check_status(exit, str_c("Could not extract frames from video file: ", x$file))
      
      # list picture files and compute their timestamp
      files <- list.files(x$temp_pics_dir)
      timestamps <- x$begin + start_offset_num + seq(from=0, by=1/fps, length.out=length(files))
      pics <- data.frame(origFile=files, dateTime=timestamps, stringsAsFactors=FALSE)
      
      return(pics)
    }, .parallel=parallel)
    if ( parallel & n > 1) {
      doParallel::stopImplicitCluster()
    }
        
    # create complete video file
    if (verbose) dmessage("create complete video")
    
    dest_video <- str_c(dir, ".mp4")
    # when there is only one video, just rename it
    if (n == 1) {
      file.rename(ds$temp_video_file, dest_video)
    # when there are several videos, join them
    } else {
      # prepare the file list for ffmpeg
      video_list_file <- make_path(temp, "list.txt")
      cat(str_c("file '", ds$temp_video_file, "'"), file=video_list_file, sep="\n")
      # concatenate files
      exit <- system2("ffmpeg", str_c("-f concat -safe 0 -i \"", video_list_file, "\" -c copy -an \"", dest_video,"\""), stdout=FALSE, stderr=ifelse(verbose, "", FALSE))
      check_status(exit, str_c("Could not concatenate files: ", str_c(ds$temp_video_file, collapse=",")))
    }

    # create complete pics directory
    if (verbose) dmessage("collect all video frames")
    
    dest_pics <- str_replace(dir, "vids", "pics")

    # reformat the log to match the columns when taking pictures from the GoPro
    pics$origFile <- make_path(pics$temp_pics_dir, pics$origFile)
    pics <- pics[,c("origFile", "dateTime")]

    # number pictures sequentially
    pics$imgNb <- 1:nrow(pics)
    pics$file <- make_path(dest_pics, str_c(pics$imgNb, ".jpg"))

    # inform the user about the number of pictures
    show_nb_records(pics, dest_pics)

    # and write it to the deployment folder
    write.csv(pics, file=str_c(dest_pics, "_log.csv"), row.names=FALSE)

    # create the pictures directory for this deployment
    dir.create(dest_pics)

    # copy the original images into their destination directory
    exit <- file.copy(pics$origFile, pics$file)
    check_status(any(!exit), str_c(sum(!exit), " frames could not be copied to ", dir))
    # and process them if needed
    process_images(pics$file, width=width, gray=gray, parallel=parallel)
    
    
    # clean up the temporary directory
    unlink(dir, recursive=TRUE)
  }
}


#' @rdname disc_extract
#' @export
#' @inheritParams disc_extract.hydrophoneRemora
disc_extract.hydrophoneRemora <- function(data, start, stop, dir, verbose=FALSE, parallel=TRUE, ...) {
  
  # compute time interval for each .wav file
  # NB: remora hydrophone cut files in ~ 4hr portions
  d <- tidyr::spread(data, key="type", value="dateTime")
  d <- dplyr::arrange(d, begin)
  d$interval <- lubridate::interval(d$begin, d$end)
  
  # detect which .wav file the deployment overlaps with
  deployInterval <- lubridate::interval(start, stop)
  ds <- d[lubridate::int_overlaps(deployInterval, d$interval),]
  show_nb_records(ds, dir)
  n <- nrow(ds)
  
  # from each .wav file, we extract the piece we need
  # NB: 
  # when the deployment overlaps two .wav files, we do not concatenate the .wav files then extract the full deployment because that (i) is longer, (ii) creates a large, useless, temporary file, (iii) looses track of the start time of the .wav file
  # => we treat each .wav file independently
  if (n < 1) {
    stop("No .wav file matches this deployment. Check your deployment log.")
  } else {
    # check time difference between .wav files if there are more than 1
    if (n > 1) {
      timeDifferences <- diff_secs(ds$begin[2:n], ds$end[1:(n-1)])
      # detect jumps
      if (any(timeDifferences > 2) ) {
        stop("Gap of more than 2 seconds between .wav files that are supposed to span the same deployment. This should not happen. Check your deployments log.")
      }
    }
    
    # create directory for output .wav files
    dir.create(dir)
    # and temporary storage
    temp <- make_path(dir, "temp")
    dir.create(temp)
    # TODO this could just be dir
    
    # record the order of files arranged by begin time
    # (used later on to define temporary names etc.)
    ds$order <- 1:n
    # prepare temporary file name(s) for the file we will cut from the original .wav file
    ds$temp_wav_file <- make_path(temp, str_c("audio", ds$order, ".wav"))
    if (verbose) print(ds)
    
    # process each source .wav files (in parallel when there are more than 1)
    # = cut the .wav file
    #   extract frames, compute time stamps and return the data
    if ( parallel & n > 1) {
      doParallel::registerDoParallel(cores=min(n, parallel::detectCores()-1))
    }
    wavs <- plyr::adply(ds, 1, function(x) {
      ## Cut source .wav file
      if (verbose) dmessage("cut wav file: ", x$file)
      
      # start of the deployment from the start of the current .wav file
      if (start > x$begin) {
        start_offset_num <- diff_secs(start, x$begin)
        start_offset <- str_c("-ss ", start_offset_num)
      } else {
        start_offset_num <- 0
        start_offset <- NULL
      }
      
      # end of the deployment from the *start* of the current .wav file
      # (i.e. duration of .wav file to extract)
      if (stop < x$end) {
        stop_offset <- str_c("-t ", diff_secs(stop, x$begin))
      } else {
        stop_offset <- NULL
      }
      
      # cut the source .wav file
      exit <- system2("ffmpeg", str_c(" -accurate_seek ", start_offset, " -i \"", x$file, "\" ", stop_offset, " -c copy -an \"", x$temp_wav_file, "\""), stdout=FALSE, stderr=ifelse(verbose, "", FALSE))
      check_status(exit, str_c("Could not cut .wav file: ", x$file))
      
      return(wavs)
    }, .parallel=parallel)
    if ( parallel & n > 1) {
      doParallel::stopImplicitCluster()
    }
    
    # create complete ,wav file
    if (verbose) dmessage("create complete .wav file")
    
    dest_wav <- str_c(dir, ".wav")
    # when there is only one file, just rename it
    if (n == 1) {
      file.rename(ds$temp_wav_file, dest_wav)
      # when there are several files, join them
    } else {
      # prepare the file list for ffmpeg
      wav_list_file <- make_path(temp, "list.txt")
      cat(str_c("file '", ds$temp_wav_file, "'"), file=wav_list_file, sep="\n")
      # concatenate files
      exit <- system2("ffmpeg", str_c("-f concat -safe 0 -i \"", wav_list_file, "\" -c copy -an \"", dest_wav,"\""), stdout=FALSE, stderr=ifelse(verbose, "", FALSE))
      check_status(exit, str_c("Could not concatenate files: ", str_c(ds$temp_wav_file, collapse=",")))
    }
    
    # clean up the temporary directory
    unlink(dir, recursive=TRUE)
  }
}
