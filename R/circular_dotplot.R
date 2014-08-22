#' Circular dotplot
#'
#' @param x vector of angles (potentially of class circular). Assumed to be in degrees and within [0,360]
# TODO relax this assumption
#' @param bin bin width in degrees
#' @param ... passed to \code{geom_point}
#'
#' @export
#' @importFrom plyr round_any count adply
#' @import ggplot2
circular_dotplot <- function(x, bin=5, ...) {
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

