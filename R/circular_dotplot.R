#' Circular dotplot
#'
#' @param x vector of angles, assumed to be bearings (in degrees, from North). A simple numeric vector is supplied, it is assumed to follow the conventions of bearings. When a vector of class "\code{circular}" is supplied, it is converted to bearings
#' @param bin bin width in degrees
#' @param ... passed to \code{\link[ggplot2]{geom_point}}
#'
#' @export
#' @importFrom plyr round_any count adply
#' @import ggplot2
#'
#' @seealso \code{\link{as.bearing}} for the conversion in bearings
#'
#' @examples
#' circular_dotplot(rnorm(100, 0, 20))
#' library("circular")
#' circular_dotplot(rvonmises(100, 0, 10))
#' circular_dotplot(rvonmises(100, pi/2, 10))
circular_dotplot <- function(x, bin=5, ...) {
  # make sure these are bearings
  x <- as.bearing(x)

  # bin angles
  xB <- as.numeric(round_any(x, bin))
  xB[xB==360] <- 0

  # create a data.frame with count
  counts <- count(xB)

  # repeat each point the appropriate number of times
  d <- adply(counts, 1, function(x) {
    data.frame(x=x$x, count=1:x$freq)
  }, .expand=F)

  # make the scale prettier
  d$count <- 10 + d$count

  p <- ggplot(d) +
  	geom_point(aes(x=x, y=count), ...) +
    scale_y_continuous(limits=c(0, max(d$count))) +
  	polar()

  return(p)
}

