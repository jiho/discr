#' Set or convert angles conventions
#'
#' @param x vector of angles; either a numeric vector, in which case numbers are assumed to be angles which follow the convention of the function used (trigonometric angles, bearings, etc.); or a vector of class circular, in which case the angles will be converted from their current convention to the convention of the function used
#'
#' @details \describe{
#' \item{as.bearing}{Bearings are angles from North, always positive, measured clockwise, in degrees}
#' \item{as.heading}{Headings are angles from North, positive in the clockwise direction, in degrees, always in [-180;180]}
#' \item{as.trig}{Trigonometric angles are angles from the horizontal, measured counter-clockwise, in radians}
#' \item{as.angle}{"Angles" are trigonometric angles but in degrees}
#' }
#'
#' @seealso \code{\link[circular]{circular}} and \code{\link[circular]{conversion.circular}} in package \code{circular}
#'
#' @name angles
#'
#' @examples
#' as.trig(pi/2)
#' as.bearing(90)
#' as.angle(90)
#'
#' as.angle(as.trig(pi/2))
#' as.bearing(as.trig(pi/2))
#' as.bearing(as.trig(0))
#' as.trig(as.bearing(0))
#' as.trig(as.bearing(90))
NULL

#' @rdname angles
#' @export
#' @importFrom circular is.circular circular conversion.circular
as.bearing <- function(x) {
  if ( ! is.circular(x) ) {
    # cast to circular type when not circular, assuming the angles are indeed following the proper conventions
    x <- circular(x, units="degrees", template="geographics", modulo="2pi")
  } else {
    # convert a circular-classed angle to the geographic conventions
    x <- conversion.circular(x, units="degrees", template="geographics", modulo="2pi")
  }
  return(x)
}

#' @rdname angles
#' @export
as.heading <- function(x) {
  x <- as.bearing(x)
  x <- ifelse(x > 180, x-360, x)
  return(x)
}

#' @rdname angles
#' @export
#' @importFrom circular is.circular circular conversion.circular
as.trig <- function(x) {
  if ( ! is.circular(x) ) {
    # cast to circular type when not circular, assuming the angles are indeed following the proper conventions
    x <- circular(x)
  } else {
    # convert a circular-classed angle to the trigonometric conventions
    x <- conversion.circular(x, units="radians", template="none", modulo="2pi", zero=0, rotation="counter")
  }
  return(x)
}

#' @rdname angles
#' @export
#' @importFrom circular is.circular circular conversion.circular
as.angle <- function(x) {
  if ( ! is.circular(x) ) {
    # cast to circular type when not circular, assuming the angles are indeed following the proper conventions
    x <- circular(x, units="degrees")
  } else {
    # convert a circular-classed angle to the conventions, using degrees as the unit
    x <- conversion.circular(x, units="degrees", template="none", modulo="2pi", zero=0, rotation="counter")
  }
  return(x)
}

#' @importFrom circular circularp circularp<-
from.below <- function(x) {
  # TODO generalise this to reverse the rotation of any angle

  # Switch the direction of rotation when we look at things from below
  # Indeed, in that case the East appears to be on the left
  # This computes the symmetry and puts E and W where they belong

  if ( ! is.circular(x) ) {
    stop("x needs to be of class 'circular'")
  }

  # _set_ the direction of measure to counter clockwise
  # (does not alter the numbers, just the attributes)
  x <- as.bearing(x)
  a <- circularp(x)
  a$rotation <- "counter"
  circularp(x) <- a

  # _convert_ back to clockwise (this actually changes the numbers and computes the symmetry)
  x <- as.bearing(conversion.circular(x, units="degrees", rotation="clock"))

  return(x)
}


#' Convert from cardinal to polar coordinates
#'
#' @param x matrix or data frame with columns [x,y]
#' @param orig vector with the x,y coordinates of the origin, defaults to (0,0)
#'
#' @return a data.frame with columns [theta,rho], with theta following trigonometric conventions and rho in the same unit as the input x and y
#'
#' @export
#' @seealso \code{link{pol2car}} for the reverse
#'
#' @examples
#' car2pol(data.frame(x=1, y=1))
#' car2pol(data.frame(1, 0))
#' car2pol(data.frame(-1, 0))
car2pol <- function (x, orig=c(0,0)) {

  # make the coordinates relative to the origin
  x[,1] <- x[,1] - orig[1]
  x[,2] <- x[,2] - orig[2]

  # calculate the angles (modulo 2*pi)
  theta <- atan2(x[,2], x[,1])
  theta <- ( theta + 2*pi ) %% ( 2*pi )
  theta <- as.trig(theta)

  # calculate the norms
  rho <- sqrt(x[,1]^2 + x[,2]^2)

  # create the matrix of polar coordinates
  inpol <- data.frame(theta, rho)

  return(inpol)
}

#' Convert from polar to cardinal coordinates
#'
#' @param x matrix or data frame with columns [theta,rho], with theta of class circular or in trigonometric reference
#' @param orig vector with the x,y coordinates of the origin, defaults to (0,0)
#'
#' @return a data.frame with columns [x,y] in the same unit as rho
#'
#' @export
#'
#' @seealso \code{link{car2pol}} for the reverse
#'
#' @examples
#' pol2car(data.frame(theta=pi/2, rho=1))
#' pol2car(data.frame(pi, 1))
#' pol2car(data.frame(as.bearing(90), 1))
pol2car <- function (x, orig=c(0,0)) {

  # make sure angles are in the trigonometric convention
  x[,1] <- as.trig(x[,1])

  # compute cartesian coordinates
  X <- x[,2] * as.numeric(cos(x[,1]))
  Y <- x[,2] * as.numeric(sin(x[,1]))
  
  # zero very small numbers
  X[which(abs(X) <= .Machine$double.eps)] <- 0
  Y[which(abs(Y) <= .Machine$double.eps)] <- 0

  # make the coordinates relative to the origin
  X <- X + orig[1]
  Y <- Y + orig[2]

  incar <- data.frame(x=X, y=Y)

  return(incar)
}
