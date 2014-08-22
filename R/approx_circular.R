# "Linearly" interpolates angles along a circle
# x     "coordinates" (e.g. time of measurement) of the angles to be interpolated
# angles  angles to be interpolated, of class circular or in trigonometric reference
# xout    "coordinates" where the interpolation should take place
# ...   passed to approx
#
#' @importFrom circular is.circular conversion.circular
approx_circular <- function(x, angles, xout, ...) {
  # Get circular characteristics of the angles object if it is of class circular
  # so that we can set them back on the resulting angles
  if ( is.circular(angles) ) {
    a <- attributes(angles)$circularp
  }

  # Convert angles to cardinal coordinates
  incar <- pol2car(data.frame(angles,1))

  # Interpolate each cardinal component independently
  xInterp <- approx(x, incar[,1], xout, ...)
  yInterp <- approx(x, incar[,2], xout, ...)

  # Convert back in polar coordinates
  inpol <- car2pol(data.frame(xInterp$y, yInterp$y))

  # Convert the resulting angles to the same circular attributes
  if ( is.circular(angles) ) {
    inpol$theta <- conversion.circular(inpol$theta, type=a$type, units=a$units, template=a$template, modulo=a$modulo, zero=a$zero, rotation=a$rotation)
  }

  return(list(x=xInterp$x, y=inpol$theta))
}
