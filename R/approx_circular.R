#' "Linearly" interpolates angles along a circle
#'
#' Interpolate angles by converting them to cardinal coordinates, linearly interpolating positions in cardinal space (x, y) and converting the resulting points back into polar space (i.e. angles). Please note that this will not work for steps of angles >= pi in the input vector of angles; but this is very difficult to check in a general way so no check is made
#'
#' @param x "coordinate" (e.g. time of measurement) of the angles to be interpolated
#' @param angles angles to be interpolated, of class circular or in trigonometric reference
#' @param xout "coordinates" where the interpolation should take place
#' @param ... passed to approx
#'
#' @return A list with components x, containing the output coordinate (xout), and y, containing the interpolated angles (in the same circular reference as the input angles)
#'
#' @export
#'
#' @examples
#' x <- c(0, 1)
#' y <- c(0, pi/2)
#' approx_circular(x=x, angles=y, xout=c(0, .5, 1))
#'
#' library("circular")
#' y <- circular(c(0, 90), template="geographics", units="degrees")
#' approx_circular(x=x, angles=y, xout=c(0, .5, 1))
#'
#' # this works
#' x <- c(0, 1, 2)
#' y <- c(0, 0.7*pi, 1.5*pi)
#' approx_circular(x=x, angles=y, xout=c(0, .5, 1, 1.5, 2))
#'
#' # but gives incorrect results because one step is larger than pi
#' y <- c(0, 0.4*pi, 1.5*pi)
#' approx_circular(x=x, angles=y, xout=c(0, .5, 1, 1.5, 2))
#' y <- c(1.5*pi, 0.6*pi, 0.7*pi)
#' approx_circular(x=x, angles=y, xout=c(0, .5, 1, 1.5, 2))
approx_circular <- function(x, angles, xout, ...) {
  # Get circular characteristics of the angles object if it is of class circular
  # so that we can set them back on the resulting angles
  inputIsCircular <- circular::is.circular(angles)
  if ( inputIsCircular ) {
    a <- attributes(angles)$circularp
  }

  # TODO check for >= pi steps
  # # Check the steps are not too large
  # angles <- as.trig(angles)
  # # NB: it is easier to do with bearing which are always positive
  # steps <- diff(as.bearing(angles))
  # actually the rotation is screwed up now...
  # if (any(abs(steps) >= 180)) {
  #   warning("One step in input angles is >= pi, approx_circular will be wrong")
  # }

  # Convert angles to cardinal coordinates
  incar <- pol2car(data.frame(angles,1))

  # Interpolate each cardinal component independently
  xInterp <- stats::approx(x, incar[,1], xout, ...)
  yInterp <- stats::approx(x, incar[,2], xout, ...)

  # Convert back in polar coordinates
  inpol <- car2pol(data.frame(xInterp$y, yInterp$y))

  # Convert the resulting angles to the same circular attributes
  if ( inputIsCircular ) {
    inpol$theta <- circular::conversion.circular(inpol$theta, type=a$type, units=a$units, template=a$template, modulo=a$modulo, zero=a$zero, rotation=a$rotation)
  }

  return(list(x=xInterp$x, y=inpol$theta))
}
