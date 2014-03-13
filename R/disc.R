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
disc <- function(ids, actions=c("calibrate", "track", "compass", "stats"), wd=getOption("disc.wd"), ...) {
  
  # check if working directoy is set
  if (is.null(wd)) {
    stop("Working directory not provided. Please provide it in the `wd` argument or set it permanently with `disc_setwd`")
  }
  
  # get/set disc options
  disc_conf()

  # get actions
  actions <- match.arg(actions, several.ok=TRUE)
  
  # check ids
  existingDeployments <- list.dirs(wd, full.names=FALSE, recursive=FALSE)
  ok <- ids %in% existingDeployments
  if (any(!ok)) {
    warning("Deployments ", str_c(ids[!ok], collapse=", "), " were not found and have been skipped")
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