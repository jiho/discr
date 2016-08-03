#' Extract data for each deployment
#'
#' Read the deployment logs, read necessary data in the raw data directory, extract data for each deployment based on date and time
#'
#' @param raw path to the directory where the raw data and the deployment and leg logs are; by default in a subdirectory called \code{raw} in the current directory
#' @param ids deployment identifiers to extract; if NULL (the default) extract all deployments
#' @param acclimation.time duration of the acclimation time in minutes.
#' @param observation.time duration of the observation period (after acclimation) in minutes.
#' @param width width to resize the images to, in pixels. When NULL, images are not resized.
#' @param split.pics whether to create pictures in the deployments directory. TRUE by default but it can be useful to set it to false to quickly recreate the rest of the metadata (since the pictures are the longest to process)
#' @param parallel do the resizing/copying of images in parallel (on n-1 processor cores) to speed up the process
#' @inheritParams disc_dd
#'
#' @export
#' @importFrom stringr str_c
#'
#' @examples
#' # get an example dataset included with the package
#' source <- system.file("extdata", "raw", package = "discr")
#' dest   <- tempdir()
#' disc_extract_deployments(raw=source, deploy.dir=dest, width=NULL,
#'                          acclimation.time=2, observation.time=2, parallel=FALSE)
#' # NOTE:
#' # - the warning about deployment 2 being stopped earlier than the expected 2 mins
#' # - the notice that the compass (cc) has 0 records in deployment 2
#'
#' system(paste0("ls -R ", dest))
disc_extract_deployments <- function(raw="raw", ids=NULL, deploy.dir=NULL, acclimation.time=5, observation.time=15, width=1600, split.pics=TRUE, parallel=TRUE) {
  ## Prepare arguments ----
  
  # check directory arguments
  # raw source
  if (!file.exists(raw)) {
    stop("Cannot find raw data directory: ", raw)
  }
  # destination directory
  dest <- disc_dd(deploy.dir)

  # read logs
  message("Reading field logs")
  legLog <- read.csv(make_path(raw, "leg_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  deployLog <- read.csv(make_path(raw, "deployment_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))

  # check deployment ids
  if ( any(duplicated(deployLog$deploy_id)) ) {
    stop("Deployment ids need to be unique. Check your deployment log")
  }
  if ( is.null(ids) ) {
    # if no deployment ids are specified, keep them all
    ids <- deployLog$deploy_id
  } else {
    # if some are specified, verify they are valid
    unknownIds <- setdiff(ids, deployLog$deploy_id)
    if (length(unknownIds) > 0) {
      warning("Deployments ", str_c(unknownIds, collapse=", "), " do not exist and will be ignored.")
    }
  }
  # select appropriate deployment ids and corresponding legs
  deployLog <- dplyr::filter(deployLog, deploy_id %in% ids)
  legLog <- dplyr::filter(legLog, leg %in% deployLog$leg)

  # detect sensors
  # sensors are specified in leg_log.csv in the form "sensor name"_"info category"
  # usual infos are : start, stop, dir, offset
  colNames <- names(legLog)
  sensorColumns <- colNames[stringr::str_detect(colNames, "_")]
  sensorInfo <- stringr::str_split_fixed(sensorColumns, "_", 2)
  sensors <- unique(sensorInfo[,1])
  infos <- unique(sensorInfo[,2])

  # make sure all columns are present (and fill the missing ones with NA)
  allSensorColumns <- str_c(rep(sensors, each=length(infos)), infos, sep="_")
  legLog[,setdiff(allSensorColumns, names(legLog))] <- NA
  # fill missing infos with defaults
  for (sensor in sensors) {
    # if the sensor-specific directory is not provided, use the name of the sensor
    col <- str_c(sensor, "_dir")
    legLog[,col][is.na(legLog[,col])] <- sensor

    # if the offset is not provided, set it to 0
    col <- str_c(sensor, "_offset")
    legLog[,col][is.na(legLog[,col])] <- 0
  }

  ## Read data or metadata for all sensors ----
  
  # read the data for the appropriate legs and correct the time stamps
  message("Inspecting data from each sensor in each leg", appendLF=FALSE)
  sensorDataList <- plyr::llply(sensors, function(sensor) {
    message("\n  ", format(sensor, width=10), appendLF=FALSE)

    # read the data per leg because there is a time shift to be done per leg
    D <- plyr::ddply(legLog, ~leg, function(dl) {
      cat(".")
      # get the folder in which the data is
      sensorDirName <- dl[,str_c(sensor,"_dir")]
      dataDir <- make_path(raw, dl$leg, sensorDirName)

      if ( file.exists(dataDir) ) {
        # read the data in the folder using the appropriate method
        # the method needs to be defined for each sensor, by the user
        class(dataDir) <- sensor
        d <- disc_read(dataDir)

        # shift the time by the offset for this sensor
        d$dateTime <- d$dateTime + dl[,str_c(sensor,"_offset")]
      } else {
        d <- NULL
      }

      return(d)
    })
    # remove the leg identifier because we don't need it
    D <- D[,-1]

    return(D)
  })
  cat("\n")
  names(sensorDataList) <- sensors

  ## Split into deployments ----
  log <- dplyr::left_join(deployLog, legLog, by="leg")
  
  plyr::d_ply(log, ~deploy_id, function(x) {

    # create the deployment folder
    deployDir <- make_path(dest, x$deploy_id)
    message("Extracting to ", deployDir)
    # when the deployment exists, move it around
    if (file.exists(deployDir)) {
      # move the data to a directory with the prefix "old."
      oldDeployDir <- make_path(dest, str_c("old.", x$deploy_id))
      dir.create(oldDeployDir, showWarnings=FALSE)
      file.copy(deployDir, oldDeployDir, recursive=TRUE)
      disc_message("Deployment ", basename(deployDir), " exists !!! Moving it to ", basename(oldDeployDir))
    } else {
      dir.create(deployDir, showWarnings=FALSE, recursive=TRUE)
    }

    # get start and stop time for this deployment
    start <- parse_date_time(str_c(x$date_start, " ", x$time_start), orders="ymd hms")
    start <- start + acclimation.time * 60
    stopTheoretical <- start + observation.time * 60
    # check that the stop time is compatible with the deployment duration
    stopRecorded <- parse_date_time(str_c(x$date_stop, " ", x$time_stop), orders="ymd hms")
    if ( stopTheoretical > stopRecorded ) {
      warning("Deployment ", x$deploy_id, " was stopped early.", call.=FALSE)
      stop <- stopRecorded
    } else {
      stop <- stopTheoretical
    }
    duration <- difftime(stop, start, units="mins")
    disc_message(start, " -> ", stop, " = ", duration, " mins")

    # for all sensors
    plyr::l_ply(sensors, function(sensor) {
      # select the portion of the data for this deployment
      d <- D[[sensor]]
      dc <- d[d$dateTime >= start & d$dateTime <= stop,]
      n <- nrow(dc)
      disc_message(format(sensor, width=10), " ", n, " records", if(n==0) { " !!!" })

      if (n > 1) {
        # get the folder name in which the data was
        # by convention, pictures are in .files$pictures, compass data is in "compass"
        sensorDirName <- x[,str_c(sensor, "_dir")]

        # for pictures, resize the images and number them sequentially
        if ( sensorDirName == .files$pictures & split.pics ) {
          picsDir <- make_path(deployDir, sensorDirName)
          dir.create(picsDir, showWarnings=FALSE)
          dc$imgNb <- 1:nrow(dc)
          dc$file <- make_path(picsDir, str_c(dc$imgNb, ".jpg"))
          if (parallel) {
            registerDoParallel(detectCores()-1)
          }
          a_ply(dc, 1, function(x) {
            if (is.null(width)) {
              file.copy(x$origFile, x$file)
            } else {
              system(str_c("convert -resize ", width,"x \"", x$origFile, "\" \"", x$file, "\""))
            }
          }, .parallel=parallel)
        }

        # write the selected portion of the data to the deployment folder
        write.csv(dc, file=make_path(deployDir, str_c(sensorDirName, "_log.csv")), row.names=FALSE)
      }
    })
  })

  message("\nDone")

  return(invisible(dest))
}

