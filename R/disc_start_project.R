#' Create a new project architecture
#'
#' @param path path where the project is to be created. Current directory by default
#' @param name name of the project. 'DISC' by default
#' 
#' @importFrom stringr str_c
#' @export
disc_start_project <- function(path=".", name="DISC", ...) {

  if ( ! dir.exists(path) ) {
    stop(path, " does not exist or is not a directory")
  }
  template <- system.file("extdata", "DISC", package = "discr")

  status <- file.copy(template, path, recursive=TRUE)
  if ( ! status) {
    stop("Could not create project at ", path)
  }

  source <- str_c(path, "/DISC")
  dest <- str_c(path, "/", name)
  status <- file.rename(source, dest)
  if ( ! status) {
    stop("Error in naming your project: ", name, ". The project is at: ", source)
  }
  
  message("Successfully created project at: ", dest)
  message("Now restart R in this directory (if you use RStudio, just double click the DISC.Rproj file that was created)")

  return(invisible(dest))
}
