#' Run a disc analysis action
#'
#' @param ids vector of deployments ids (correspond to folders in the working directory)
#' @param actions vector of actions to perform. Action names can be abbreviated
#' @param wd working directory of the project (where deployments are)
#' @param ... passed to the various actions functions
#'
#' @seealso \code{\link{disc_setwd}} to set the working directory
#' @export
#' @importFrom stringr str_c
disc <- function(ids, actions=c("calibrate", "track", "compass", "stats"), ...) {
  
  # get working directory
  wd <- disc_getwd()
  # TODO also make it possible to set it in the arguments (but requires to check for existence etc in that case)
  
  # get/set disc options
  disc_conf()

  # get actions
  actions <- match.arg(actions, several.ok=TRUE)
  
  # check ids
  existingDeployments <- list.dirs(wd, full.names=FALSE, recursive=FALSE)
  ok <- ids %in% existingDeployments
  if (any(!ok)) {
    warning("Deployments ", str_c(ids[!ok], collapse=", "), " were not found and will be skipped", immediate.=TRUE)
    ids <- ids[ok]
  }
  
  deploymentDirectories <- make_path(wd, ids)
  
  # run actions for all ids
  for(dir in deploymentDirectories) {
    message(dir)
    if ("calibrate" %in% actions) {
      disc_calibrate(dir=dir, ...)
    }
  }
  
  message("Done")

  return(invisible(NULL))
}