#
#      Give status information about the deployments directory
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------


disc_status <- function(dir="deployments") {

  suppressPackageStartupMessages(require("plyr", quietly=TRUE))
  
  deploys <- sort(as.numeric(list.files(dir, pattern="[0-9]*")))
  
  d <- adply(deploys, 1, function(i) {
    
  	# test the existence of video
    video <- file.exists(make_path(dir, i, "video_hifi.mov"))

  	# count images
    pics <- list.files(make_path(dir, i, "pics"), pattern=glob2rx("*.jpg"))
    nbPics <- length(pics)

  	# test the existence of data files
    autoCompass <- file.exists(make_path(dir, i, "compass_log.csv"))
    manuCompass <- file.exists(make_path(dir, i, "compass_track.txt")) & file.exists(make_path(dir, i, "coord_compass.txt"))
    if (autoCompass) {
      compass <- "auto"
    } else if (manuCompass) {
      compass <- "man"
    } else {
      compass <- FALSE
    }
    ctd <- file.exists(make_path(dir, i, "ctd_log.csv"))
    gps <- file.exists(make_path(dir, i, "gps_log.csv"))

  	# test the existence of data files
    calib <- file.exists(make_path(dir, i, "coord_aquarium.csv"))
    rawTracks <- file.exists(make_path(dir, i, "larvae_tracks.txt"))
    correctedTracks <- file.exists(make_path(dir, i, "tracks.csv"))
    stats <- file.exists(make_path(dir, i, "stats.csv"))
    if (stats) {
      status <- TRUE
    } else if (correctedTracks) {
      status <- "correction done"
    } else if (rawTracks) {
      status <- "tracking done"
    } else if (calib) {
      status <- "calibration done"
    } else {
      status <- FALSE
    }

    d <- data.frame(video, pics=nbPics, compass, ctd, gps, status, stringsAsFactors=FALSE)
  })
  d <- d[,-1]
  row.names(d) <- deploys

  class(d) <- c("disc_status", "data.frame")
  
  return(d)
  
#   # when the corrected track is available, use the times there
#   if [[ -e $work/$i/tracks.csv ]]; then
#     # with R read the corrected tracks and compute average time difference
#     R -q --slave << EOF
#       # read only the beginning of the file to speed things up
#       t = read.table("${work}/${i}/tracks.csv", header=T, sep=",", as.is=T, nrows=400)
#       # set the correct time class and remove missing values
#       imgTimes = as.POSIXct(na.omit(t\$date))
#       # compute the average time difference
#       cat(sprintf("%4i",round(mean(diff(imgTimes)))))
# EOF
# 
#   # otherwise fall back on the uncorrected track
#   # but in this case times have to be extracted from the image files
#   elif [[ -e $work/$i/larvae_track.txt ]]; then
#     # read the track, get the numbers of the images on which a larva was detected
#     # select the first 3 lines, remove the header, and select 4th column
#     imgNbs=$(head -n 3 $work/$i/larvae_track.txt | sed \1d | awk -F "\t" {'print $4'})
#     # this selects only the first 2 images but using more slows things down when exiftool reads the creation date
# 
#     # re-construct the full paths to images
#     images=$(echo $imgNbs | awk '{OFS=""; print pics,$1,".jpg ",pics,$2,".jpg"}' pics="${work}/${i}/pics/")
# 
#     # compute the time difference in R
#     R -q --slave << EOF
#       # get images capture times with exiftool
#       imgTimes = system("exiftool -T -CreateDate ${images}", intern=T)
#       # compute the time difference
#       imgTimes = as.POSIXct(imgTimes,format="%Y:%m:%d %H:%M:%S")
#       cat(sprintf("%4i",as.numeric(diff(imgTimes))))
# EOF
	
#   # display subsample interval
#   if [[ -e $work/$i/stats.csv ]]; then
#     R -q --slave << EOF
#     # read the stats
#     s = read.table("${work}/${i}/stats.csv", header=T, sep=",", as.is=T, nrows=1)
#     if (is.na(s[1,"resample.lag"])) {
#       cat("  - ")
#     } else {
#       cat(sprintf("%4i",s[1,"resample.lag"]))
#     }
# EOF
#     echo -n ""
#   else
#     echo -n "    "
#   fi
# 
#   echo ""
 
}


print.disc_status <- function(x) {
  x[x==FALSE] <- "-"
  x[x==TRUE] <- "✔"
  print.data.frame(x)
  return(invisible(x))
}