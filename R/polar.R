#' Polar coordinates in ggplot
#'
#' \code{polar()} set polar coordinates and appropriate scales and \code{scale_x_circular()} only sets the scale, in a ggplot
#'
#' @param template "geographics" for bearings, "none" or "trigonometrics" for trigonometric angles; can be abbreviated
#' @param ... passed to \code{\link[ggplot2]{scale_x_continuous}}
#'
#' @import ggplot2
#' @export
#'
#' @seealso \code{\link[circular]{circular}} and \code{\link[ggplot2]{scale_x_continuous}}
#'
#' @examples
#' library("ggplot2")
#' d <- data.frame(x=runif(10, 0, 360), y=runif(10))
#' ggplot(d) + geom_point(aes(x=x, y=y)) 
#' ggplot(d) + geom_point(aes(x=x, y=y)) + polar() 
#'
#' d <- data.frame(x=runif(10, 0, 2*pi), y=runif(10))
#' ggplot(d) + geom_point(aes(x=x, y=y)) + polar(template="none")
polar <- function(template="geographics", ...) {
  list(coord_polar(theta="x"), scale_x_circular(template=template, ...))
}

#' @import ggplot2
#' @export
#' @rdname polar
scale_x_circular <- function(template="geographics", ...) {
  template <- match.arg(template, c("geographics", "none", "trigonometrics"))

  if (template == "geographics") {

    # set the scale for compass bearings
    scale = scale_x_continuous("", limits=c(0,360),
                        breaks=seq(0,360-1,by=90),
                        labels=c("N", "E", "S", "W"),
                        ...)
                        # breaks=seq(0,360-1,by=45),
                        # labels=c("N","N-E","E","S-E","S","S-W","W","N-W"))

  } else {

    # set the scale for trigonometric angles
    scale = scale_x_continuous("", limits=c(0,2*pi),
                        breaks=seq(0, 2*pi-0.001 , by=pi/2),
                        labels=c("0", expression(frac(pi,2)), expression(pi), expression(frac(3*pi,2))),
                        ...)

  }
  return(scale)
}


