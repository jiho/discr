#' Extract data for each deployment
#'
#' Read the leg and deployment logs, read necessary data in the raw data directory, extract data for each deployment based on date and time.
#'
#' @param raw path to the directory where the raw data and the deployment and leg logs are; by default in a subdirectory called \code{raw} in the current directory.
#' @param ids deployment identifiers to extract; if NULL (the default) extract all deployments.
#' @inheritParams disc_dd
#' @param acclimation.time duration of the acclimation time in minutes.
#' @param observation.time duration of the observation period (after acclimation) in minutes.
#' @param ... passed to the various \code{\link{disc_extract}} methods for each sensor.
#'
#' @details
#' To extract individual deployments from the raw leg data, \code{\link{disc_extract_deployments}}
#' \enumerate{
#'   \item{reads data (or descriptive metadata, such as start and stop time, number of records, etc.) from all sensors defined in the \code{leg_log.csv} file;}
#'   \item{synchronises all these records using the time offsets set in \code{leg_log.csv};}
#'   \item{reads \code{deployment_log.csv} and extracts the data for each deployment based on the start and stop time.}
#' }
#' 
#' To read data from a given sensor, \code{disc_extract_deployments} looks for an appropriate method for the generic function \code{\link{disc_read}}, i.e. a function named \code{disc_read.nameofsensor}. This function outputs a \code{data.frame} with at least one column called \code{dateTime} of class \code{\link[base]{POSIXct}} used to account for the offset.
#' 
#' Similarly, to extract data from a given sensor for one deployment, \code{disc_extract_deployments} looks for an appropriate method for \code{\link{disc_extract}}. The default method deals with a \code{data.frame} with a column \code{dateTime}. Other methods can be defined for data that does not fit in a \code{data.frame} (video recordings, audio recodings, etc.)
#' 
#' To read and extract data from a new sensor, one just needs to define an appropriate method for \code{\link{disc_read}} and possibly \code{\link{disc_extract}}. See the help of these two functions to know more about tem
#'
#' @seealso \code{\link{disc_read}} and \code{\link{disc_extract}}.
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
disc_extract_deployments <- function(raw="raw", ids=NULL, deploy.dir=NULL, acclimation.time=5, observation.time=15, ...) {

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
  legLog <- read_csv_auto(make_path(raw, "leg_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  deployLog <- read_csv_auto(make_path(raw, "deployment_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))

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
    message("\n  ", format(sensor, width=12), appendLF=FALSE)

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
    # when the deployment exists, archive it
    if (file.exists(deployDir)) {
      # move the data to a directory appended with its modification time
      mtime <- file.info(deployDir)$mtime
      oldDeployDir <- str_c(deployDir, "_",format(mtime, "%Y-%m-%d_%H-%M-%S"))
      file.rename(deployDir, oldDeployDir)
      warning("Deployment ", basename(deployDir), " exists. Archiving it to ", basename(oldDeployDir), call.=FALSE)
    }
    dir.create(deployDir, showWarnings=FALSE, recursive=TRUE)

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
      d <- sensorDataList[[sensor]]
      # if this sensor has data (i.e. was present), extract it
      if (nrow(d) != 0) {
        # define where it should be stored
        sensorDirName <- x[,str_c(sensor, "_dir")]
        sensorDir <- make_path(deployDir, sensorDirName)
        # extract the data
        class(d) <- c(sensor, class(d))
        disc_extract(d, start, stop, sensorDir, ...)
      }
    })
  })

  message("\nDone")

  return(invisible(dest))
}

