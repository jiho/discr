#' Run a disc analysis action
#'
#' @param ids vector of deployments ids (correspond to folders in the working directory). Usually deployment ids are integers, which makes it easy to give ranges of deployments such as 1:10. But deployment ids can be anything.
#' @param actions vector of actions to perform. Action names can be abbreviated. See below which actions are available.
#' @inheritParams disc_dd
#' @param ... passed to the various actions functions. To see which arguments are available, see the help of the various action functions
#'
#' @details \describe{
#' \item{"camera compass angle"}{estimate the angle between the top of the camera and the physical direction in which the digital compass is set up in its housing by detecting the angle of the analog compasses on a few frames and comparing that with the record of the digital compass. Is usually done on short, calibration-type deployments.}
#' \item{"calibrate"}{measure the position of the arena on the first image of a deployment, to calibrate the trajectory and convert distances from pixels on frame to mm. See the "\code{disc.diameter}" option set by \code{\link{disc_conf}}.}
#' \item{"track"}{manually track the larva(e) on each frame}
#' \item{"compass"}{manually detect the analog compass bearing when the digital compass does not work}
#' \item{"correct"}{correct larvae tracks according to compass readings, to put them back in cardinal space}
#' \item{"stats"}{compute statistics on the positions of larvae in the arena}
#' }
#'
#' @export
#' @family action functions
#' @importFrom stringr str_c
#' @importFrom gtools mixedsort
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discusr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#'
#' disc_status(dd)
#'
#' \donttest{# perform some actions on deployment 1
#' disc(ids=1, actions=c("calibrate", "track"), sub=10, deploy.dir=dd)
#' # action names can be abbreviated
#' disc(1, actions=c("cor", "st"), deploy.dir=dd)
#' # several deployments can be handled successively
#' disc(1:2, actions=c("cor", "st"), deploy.dir=dd)
#' # arguments can be passed to action functions
#' read.csv(paste0(dd, "/1/stats.csv"))
#' disc(1, actions="stats", sub=10, deploy.dir=dd)
#' read.csv(paste0(dd, "/1/stats.csv"))
#' }
disc <- function(ids=NULL, actions=c("calibrate", "track", "correct", "stats"), deploy.dir=NULL, ...) {

  # get/set deployments directory
  wd <- disc_dd(deploy.dir)

  # get/set disc options
  disc_conf()

  # get actions
  actions <- match.arg(actions, choices=c("camera compass angle", "calibrate", "track", "compass", "correct",  "stats"), several.ok=TRUE)

  # check ids
  existingDeployments <- mixedsort(list.dirs(wd, full.names=FALSE, recursive=FALSE))
  if ( is.null(ids) ) {
    ids <- existingDeployments
  }
  ok <- ids %in% existingDeployments
  if (any(!ok)) {
    warning("Deployments ", str_c(ids[!ok], collapse=", "), " were not found and will be skipped", immediate.=TRUE)
    ids <- ids[ok]
  }

  if (length(ids) >= 1) {
    deploymentDirectories <- make_path(wd, ids)

    # run actions for all ids
    for(dir in deploymentDirectories) {
      message(dir)

      if ("camera compass angle" %in% actions) {
        try(disc_camera_compass_angle(dir=dir, ...))
      }
      if ("calibrate" %in% actions) {
        try(disc_calibrate(dir=dir, ...))
      }
      if (any(c("compass", "track compass") %in% actions)) {
        try(disc_track_compass(dir=dir, ...))
      }
      if ("track" %in% actions) {
        try(disc_track(dir=dir, ...))
      }
      if ("correct" %in% actions) {
        try(disc_correct(dir=dir, ...))
      }
      if ("stats" %in% actions) {
        try(disc_stats(dir=dir, ...))
      }
    }
  }


  message("Done")

  return(invisible(NULL))
}