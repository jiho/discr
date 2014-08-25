#' Run a disc analysis action
#'
#' @param ids vector of deployments ids (correspond to folders in the working directory). Usually deployment ids are integers, which makes it easy to give ranges of deployments such as 1:10. But deployment ids can be anything.
#' @param actions vector of actions to perform. Action names can be abbreviated
# TODO list actions here
#' @inheritParams disc_dd
#' @param ... passed to the various actions functions
#'
#' @export
#' @importFrom stringr str_c
disc <- function(ids=NULL, actions=c("calibrate", "track", "correct", "stats"), deploy.dir=NULL, ...) {

  # get/set deployments directory
  wd <- disc_dd(deploy.dir)

  # get/set disc options
  disc_conf()

  # get actions
  actions <- match.arg(actions, choices=c("camera compass angle", "calibrate", "track", "compass", "track compass", "correct",  "stats"), several.ok=TRUE)

  # check ids
  existingDeployments <- list.dirs(wd, full.names=FALSE, recursive=FALSE)
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