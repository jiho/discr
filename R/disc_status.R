#' Status of the deployments directory
#'
#' Give information about deployments (number of pictures, metadata files, data files, etc.)
#'
#' @inheritParams disc_dd
#'
#' @export
#' @importFrom plyr ldply
#' @importFrom gtools mixedsort
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discr")
#'
#' disc_status(deploy.dir=deploys)
disc_status <- function(deploy.dir=NULL) {

  dir <- disc_dd(deploy.dir)

  message(dir)

  # list all deployments
  deployments <- mixedsort(list.dirs(dir, full.names=FALSE, recursive=FALSE))

  d <- ldply(deployments, function(i) {

    # test the existence of video
    video <- file.exists(make_path(dir, i, .files$video))

    # count images
    pics <- list.files(make_path(dir, i, .files$pictures), pattern=glob2rx("*.jpg"))
    nbPics <- length(pics)

    # test the existence of data files
    # compass
    digitalCompass <- file.exists(make_path(dir, i, .files$digital.compass))
    analogCompass  <- file.exists(make_path(dir, i, .files$analog.compass)) & file.exists(make_path(dir, i, .files$analog.compass.coord))
    if (digitalCompass) {
      compass <- "digi"
    } else if (analogCompass) {
      compass <- "ana"
    } else {
      compass <- FALSE
    }

    # rest
    calib   <- file.exists(make_path(dir, i, .files$aquarium.coord))
    track   <- file.exists(make_path(dir, i, .files$tracks))
    correct <- file.exists(make_path(dir, i, .files$rotated.tracks))
    stats   <- file.exists(make_path(dir, i, .files$stats))

    # list sensor data
    logFiles <- list.files(make_path(dir, i), pattern=glob2rx("*_log.csv"))
    # remove pics and compass logs which we already dealt with above
    logFiles <- setdiff(logFiles, c("pics_log.csv", .files$digital.compass, .files$analog.compass))
    # detect sensor names
    sensorNames <- str_replace(logFiles, fixed("_log.csv"), "")
    # create the sensors table
    if (length(sensorNames) > 0) {
      sensors <- matrix(TRUE, ncol=length(sensorNames))
      colnames(sensors) <- sensorNames
    } else {
      sensors <- NA
    }

    d <- data.frame(video, pics=nbPics, compass, calib, track, correct, stats, sensors, stringsAsFactors=FALSE)
  })
  row.names(d) <- deployments

  # replace NA by FALSE
  d[is.na(d[,])] <- FALSE

  # remove columns that are entirely empty/FALSE
  # those are usually non-existent sensors
  empty <- laply(d, function(x) { all(x == FALSE) } )
  d <- d[, ! empty ]

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

#' @rdname disc_status
#' @export
dstatus <- disc_status

#' @export
#' @keywords internal
print.disc_status <- function(x, ...) {
  x[x==FALSE] <- "x"
  # x[x==TRUE] <- "✔"
  x[x==TRUE] <- "."
  print.data.frame(x)
  return(invisible(x))
}