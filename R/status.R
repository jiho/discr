#' Status of the working directory
#'
#' Give information about deployments in the working directory (number of pictures, metadata files, data files, etc.)
#'
#' @param dir path to the working directory. Read from the option \code{disc.wd} by default.
#'
#' @seealso \code{\link{disc_setwd}} to set the working directory option.
#' @export
#' @importFrom plyr ldply
disc_status <- function(dir=getOption("disc.wd")) {

  if (!file.exists(dir)) {
    stop("Cannot find ", dir)
  }
  message(dir)

  # list all deployments
  deployments <- list.dirs(dir, full.names=FALSE, recursive=FALSE)

  d <- ldply(deployments, function(i) {

    # test the existence of video
    video <- file.exists(make_path(dir, i, .files$video))

    # count images
    pics <- list.files(make_path(dir, i, .files$pictures), pattern=glob2rx("*.jpg"))
    nbPics <- length(pics)

    # test the existence of data files
    digitalCompass <- file.exists(make_path(dir, i, .files$digital.compass))
    manualCompass  <- file.exists(make_path(dir, i, .files$manual.compass)) & file.exists(make_path(dir, i, .files$manual.compass.coord))
    if (digitalCompass) {
      compass <- "digi"
    } else if (manualCompass) {
      compass <- "manu"
    } else {
      compass <- FALSE
    }
    ctd <- file.exists(make_path(dir, i, .files$ctd))
    gps <- file.exists(make_path(dir, i, .files$gps))
    light <- file.exists(make_path(dir, i, .files$light))

    # test the existence of data files
    calib   <- file.exists(make_path(dir, i, .files$coord.aquarium))
    track   <- file.exists(make_path(dir, i, .files$tracks))
    correct <- file.exists(make_path(dir, i, .files$corrected.tracks))
    stats   <- file.exists(make_path(dir, i, .files$stats))
    # if (stats) {
    #   status <- TRUE
    # } else if (correctedTracks) {
    #   status <- "correction done"
    # } else if (rawTracks) {
    #   status <- "tracking done"
    # } else if (calib) {
    #   status <- "calibration done"
    # } else {
    #   status <- FALSE
    # }

    d <- data.frame(video, pics=nbPics, compass, ctd, gps, light, calib, track, correct, stats, stringsAsFactors=FALSE)
  })
  row.names(d) <- deployments

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

print.disc_status <- function(x) {
  x[x==FALSE] <- "x"
  # x[x==TRUE] <- "✔"
  x[x==TRUE] <- "."
  print.data.frame(x)
  return(invisible(x))
}