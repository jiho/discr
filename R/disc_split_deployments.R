#' Get data for deployments
#'
#' Read the deployment logs, read necessary data in the raw data directory, split data into folders per deployment, based on date and time
#'
#' @param raw path to the directory where the raw data and the deployment and daily logs are.
#' @param dest path to destination directory, where deployment folders will be stored. Will be created if it does not exist. Is set to discuss working directory by default.
#' @param ids deployment identifiers to extract; if NULL (the default) get all deployments
#' @param acclimation.time duration of the acclimation time in minutes.
#' @param observation.time duration of the observation period (after acclimation) in minutes.
#'
#' @details The csv file with the deployment log. Should have, at least, columns named 'id', 'start_date', 'start_time', 'stop_date', 'stop_time'. Date should be formatted as YYYY-MM-DD and time as HH:MM:SS. Start date and time are when the DISC is beginning to being lowered in the water. Stop date and time are when the DISC is back on the boat.
# TODO rewrite documentation and requirements in terms of log sheets
#'
#' @export
#' @importFrom stringr str_detect str_split_fixed str_c
#' @importFrom plyr ldply join d_ply l_ply adply a_ply llply
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom tools file_ext
disc_split_deployments <- function(raw, dest=disc_getwd(), ids=NULL, acclimation.time=5, observation.time=15) {

  # TODO Use .file for file names

  message("Reading field logs")
  # convert to csv first using
  # soffice --headless --convert-to csv test.ods

  # read the logs
  dailyLog <- read.csv(str_c(raw, "/daily_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  deployLog <- read.csv(str_c(raw, "/deployment_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  if ( any(duplicated(deployLog$deployId)) ) {
    stop("Deployment ids need to be unique. Check you deployment log")
  }
  # TODO check that deployment duration is compatible with acclimation.time + observation.time


  # detect sensors
  # sensors are specified in the dailyLog in the form "sensor name"_"info category"
  colNames <- names(dailyLog)
  sensorColumns <- colNames[str_detect(colNames, "_")]
  sensorInfo <- str_split_fixed(sensorColumns, "_", 2)
  sensors <- unique(sensorInfo[,1])
  infos <- unique(sensorInfo[,2])

  # fill the missing infos with defaults
  # make sure all columns are present (and fill the missing ones with NA)
  allSensorColumns <- str_c(rep(sensors, each=length(infos)), infos, sep="_")
  dailyLog[,setdiff(allSensorColumns, names(dailyLog))] <- NA
  # if the folder is not provided, use the name of the sensor
  for (sensor in sensors) {
    col <- str_c(sensor, "_folder")
    dailyLog[,col][is.na(dailyLog[,col])] <- sensor
  }
  # if the offset is not provided, set it to 0
  for (sensor in sensors) {
    col <- str_c(sensor, "_offset")
    dailyLog[,col][is.na(dailyLog[,col])] <- 0
  }


  # if no deployment id is specified, keep them all
  if ( is.null(ids) ) {
    ids <- deployLog$deployId
  }

  # select appropriate deployment ids and corresponding logs
  deployLog <- deployLog[deployLog$deployId %in% ids,]
  dailyLog <- dailyLog[dailyLog$leg %in% deployLog$leg,]


  # for all sensors, read the data for the appropriate legs and correct the time stamps
  message("Reading all data", appendLF=FALSE)
  D <- llply(sensors, function(sensor) {
    message("\n  ", format(sensor, width=10), appendLF=FALSE)

    # read the data per leg because there is a time shift to be done per leg
    D <- ddply(dailyLog, ~leg, function(dl) {
      cat(".")
      # get the folder in which the data is
      sensorFolderName <- dl[,str_c(sensor,"_folder")]
      dataFolder <- str_c(raw, "/", dl$leg, "/", sensorFolderName)

      if ( file.exists(dataFolder) ) {
        # read the data in the folder using the appropriate method
        # the method needs to be defined for each sensor, by the user
        class(dataFolder) <- sensor
        d <- disc_read(dataFolder)

        # shift the time by the offset for this sensor
        # if the offset is not provided, do not shift anything
        offset <- dl[,str_c(sensor,"_offset")]
        d$dateTime <- d$dateTime + offset
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
  names(D) <- sensors

  # split into deployments
  log <- join(deployLog, dailyLog, by="leg")
  d_ply(log, ~deployId, function(x) {
    message("Extracting deployment ", x$deployId)

    # create the deployment folder
    deployDir <- str_c(dest, "/", x$deployId)
    dir.create(deployDir, showWarnings=FALSE, recursive=TRUE)
    # TODO check the existence and warn about overwrite

    # get start and stop time
    start <- parse_date_time(str_c(x$date_start, " ", x$time_start), orders="ymd hms")
    start <- start + acclimation.time * 60
    stopTheoretical <- start + observation.time * 60
    stopRecorded <- parse_date_time(str_c(x$date_stop, " ", x$time_stop), orders="ymd hms")
    if ( stopTheoretical > stopRecorded ) {
      warning("Deployment ", x$deployId, " was stopped early.")
      stop <- stopRecorded
    } else {
      stop <- stopTheoretical
    }
    duration <- difftime(stop, start, units="mins")
    message("  ", start, " -> ", stop, " = ", duration, " mins")

    # for all sensors
    l_ply(sensors, function(sensor) {
      # select the portion of the data for this deployment
      d <- D[[sensor]]
      dc <- d[d$dateTime >= start & d$dateTime <= stop,]
      n <- nrow(dc)
      message("  ", format(sensor, width=10), " ", n, " records", if(n==0) { " !!!" })

      if (n > 1) {
        # get the folder name in which the data was
        # by convention, pictures are in "pics", compass data is in "compass"
        sensorFolderName <- x[,str_c(sensor, "_folder")]

        # for pictures, resize the images and number them sequentially
        if ( sensorFolderName == "pics" ) {
          picsDir <- str_c(deployDir, "/", sensorFolderName)
          dir.create(picsDir, showWarnings=FALSE)
          dc$imgNb <- 1:nrow(dc)
          dc$file <- str_c(picsDir, "/", dc$imgNb, ".jpg")
          registerDoParallel(detectCores()-1)
          a_ply(dc, 1, function(x) {
            system(str_c("convert -resize 1920x1280 \"", x$origFile, "\" \"", x$file, "\""))
          }, .parallel=TRUE)
          # file.copy(dc$fileName, picsDir)
        }

        # write the selected portion of the data to the deployment folder
        write.csv(dc, file=str_c(deployDir, "/", sensorFolderName, "_log.csv"), row.names=FALSE)
      }
    })
  })

  message("\nDone")
  message("Deployments are in ", dest)

  return(invisible(dest))
}

