#' Split data into deployments
#'
#' Read all data in the raw data directory, read the deployment logs, split data into folders per deployment, based on time
#'
#' @param raw path to the directory where the raw data and the deployment and daily logs are.
#' @param dest path to destination directory, where deployment folders will be stored. Will be created if it does not exist. Is set to discuss working directory by default.
#' @param acclimation.time duration of the acclimation time in minutes
#' @param observation.time duration of the observation period (after acclimation) in minutes
#'
#' @details The csv file with the deployment log. Should have, at least, columns named 'id', 'start_date', 'start_time', 'stop_date', 'stop_time'. Date should be formatted as YYYY-MM-DD and time as HH:MM:SS. Start date and time are when the DISC is beginning to being lowered in the water. Stop date and time are when the DISC is back on the boat.
# TODO rewrite documentation and requirements in terms of log sheets
#'
#' @export
#' @importFrom stringr str_detect str_split_fixed str_c
#' @importFrom plyr ldply join d_ply l_ply
#' @importFrom tools file_ext
disc_split_deployments <- function(raw, dest=disc_getwd(), acclimation.time=5, observation.time=15) {

  message("Reading field logs")
  # convert to csv first using
  # soffice --headless --convert-to csv test.ods

  # read the logs
  dailyLog <- read.csv(str_c(raw, "/daily_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  deployLog <- read.csv(str_c(raw, "/deployment_log.csv"), stringsAsFactors=FALSE, na.strings=c("NA", ""))
  # TODO check that deployId is unique
  # TODO check that deployment duration is compatible with acclimation.time + observation.time
  
  # detect sensors
  colNames <- names(dailyLog)
  sensorColumns <- colNames[str_detect(colNames, "_")]
  sensors <- unique(str_split_fixed(sensorColumns, "_", 2)[,1])

  # loop over sensors and read the corresponding data
  message("Reading all data")
  D <- llply(sensors, function(sensor) {
    message("  ", sensor)
    
    # get all files for this sensor
    sensorFileNames <- unique(dailyLog[,str_c(sensor,"_name")])
    sensorFilePaths <- list.files(raw, pattern=glob2rx(sensorFileNames), recursive=TRUE, full.names=TRUE)
    
    # if the file is a picture read its capture time in the EXIF data
    extension <- tolower(file_ext(sensorFilePaths[1]))
    if (extension %in% c("jpg", "jpeg")) {
      times <- image_time(sensorFilePaths)
      d <- data.frame(fileName=sensorFilePaths, dateTime=times, stringsAsFactors=FALSE)

    # else read all files and concatenate the data
    } else {
      d <- ldply(sensorFilePaths, function(file) {
        class(file) <- sensor
        d <- disc_read(file)
      })      
    }
  })
  names(D) <- sensors
  
  # split into deployments
  log <- join(deployLog, dailyLog, by="leg")
  d_ply(log, ~deployId, function(x) {
    message("Extracting deployment ", x$deployId)
    
    # create the deployment folder
    deployDir <- str_c(dest, "/", x$deployId)
    dir.create(deployDir, showWarnings=FALSE, recursive=TRUE)
    
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
        # find out what kind of data we have
        if ( "heading" %in% names(dc) ) {
          role <- "compass"
        } else if ( "lat" %in% names(dc) ) {
          role <- "gps"
        } else if ( "fileName" %in% names(dc) ) {
          role <- "pics"
        } else {
          role <- sensor
        }
      
        # write the data
        if ( role == "pics" ) {
          # for pictures, copy the images
          picsDir <- str_c(deployDir, "/pics")
          dir.create(picsDir, showWarnings=FALSE)
          file.copy(dc$fileName, picsDir)
        } else {
          # for data, write it as csv
          write.csv(dc, file=str_c(deployDir, "/", role, ".csv"), row.names=FALSE)
        }        
      }
    })
  })

  message("\nDone")
  message("Deployments are in ", dest)
  
  return(invisible(dest))
}

