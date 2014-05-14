# Order columns of a data.frame
#
# @param d      data.frame
# @param first  names of columns to be put first
# @param drop   when TRUE, keep only the columns in \code{first}. Otherwise put additional columns afterwards
#
#' @keywords internal
reorder_columns <- function(d, first, drop=FALSE) {

  # allow partial matching of names
  n <- names(d)
  first <- match.arg(first, n, several.ok=TRUE)

  # reorder / drop columns
  if ( drop ) {
    d <- d[ , first]
  } else {
    d <- d[ , c(first, setdiff(n, first))]
  }

  return(d)
}

# Select columns of a data.frame
#
# @param d      data.frame
# @param names  names of columns to keep, can be abbreviated
#
#' @keywords internal
select_columns <- function(d, names) {
  # allow partial matching of names
  n <- names(d)
  names <- match.arg(names, n, several.ok=TRUE)

  return(d[,names])
}

# Select columns of a data.frame
#
# @param d      data.frame
# @param names  names of columns to remove, can be abbreviated
#
#' @keywords internal
remove_columns <- function(d, names) {
  # allow partial matching of names
  n <- names(d)
  names <- match.arg(names, n, several.ok=TRUE)

  return(d[, ! n %in% names])
}