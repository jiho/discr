#' Set or convert angles between conventions
#'
#' @param x vector of angles; either as numbers, in which case they are assumed to follow the appropriate convention; or of class circular, in which case the angles will be converted from their current convention to the convention determined by the function
#'
#' @details Bearings are angles from North, always positive, measured clockwise, in degrees. Trigonometric angles are angles from the horizontal, measured counter-clockwise, in radians. "Angles" are trigonometric angles but in degrees.
# TODO itemize this
#'
#' @seealso \code{\link{circular}} and \code{\link{conversion.circular}} in package \code{circular}
#'
#' @name angles
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

#' @rdname angles
#' @export
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

  # _convert_ back to clockwise (this actually changes the numbers and compute the symmetry)
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
pol2car <- function (x, orig=c(0,0)) {

  # make sure angles are in the trigonometric convention
  x[,1] <- as.trig(x[,1])

  # compute cartesian coordinates
  X <- x[,2] * cos(x[,1])
  Y <- x[,2] * sin(x[,1])

  # make the coordinates relative to the origin
  X <- X + orig[1]
  Y <- Y + orig[2]

  incar <- data.frame(x=X, y=Y)

  return(incar)
}


# "Linearly" interpolates angles along a circle
# x     "coordinates" (e.g. time of measurement) of the angles to be interpolated
# angles  angles to be interpolated, of class circular or in trigonometric reference
# xout    "coordinates" where the interpolation should take place
# ...   passed to approx
#
#' @importFrom circular is.circular conversion.circular
approx.circular <- function(x, angles, xout, ...) {
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


#' Descriptive statistics and Rayleigh test
#'
#' @param x vector of angles, of class \code{circular}
#'
#' @return A data.frame with columns
#' \itemize{
#' \item{n}{sample size}
#' \item{mean}{mean angle}
#' \item{variance}{angular variance}
#' \item{r}{Rayleigh r}
#' \item{p.value}{p-value of Rayleigh's test}
#' }
#'
#' @export
#'
#' @importFrom circular is.circular mean.circular rayleigh.test
circ.stats <- function(x) {

  # check the class of angles
  if ( ! is.circular(x)) {
    stop("x needs to be of class circular")
  }

  # sample size
  n <- length(x)

  # mean angle
  mean <- mean.circular(x)

  # rayleigh test
  rayleigh <- rayleigh.test(x)
  r <- rayleigh$statistic
  p.value <- rayleigh$p.value
  signif <- p.value < 0.05

  # angular variance ~ variance
  #  = (1-r)
  # NB: Batschelet, 1981. Circular Statistics in Biology. p. 34 adds a multiplication by 2 compared to this formula
  variance <- 1 - r
  # variance = var.circular(angles)

  # # angular deviation ~ standard deviation
  # # = sqrt( (1-r) )
  # sd = sqrt(variance)

  return(data.frame(n, mean, variance, r, p.value, signif))
}


# Set the x scale appropriately for the given template
# template    "geographics" (for bearings) or "none" (for trigonometric angles)
# @importFrom ggplot2 scale_x_continuous
# @export
scale_x_circular <- function(template=c("geographics", "none")) {
  template <- match.arg(template)

  if (template == "geographics") {

    # set the scale for compass bearings
    scale = scale_x_continuous("", limits=c(0,360),
                        breaks=seq(0,360-1,by=90),
                        labels=c("N", "E", "S", "W"))
                        # breaks=seq(0,360-1,by=45),
                        # labels=c("N","N-E","E","S-E","S","S-W","W","N-W"))

  } else {

    # set the scale for trigonometric angles
    scale = scale_x_continuous("", limits=c(0,2*pi),
                        breaks=seq(0, 2*pi-0.001 , by=pi/2),
                        labels=c("0", expression(frac(pi,2)), expression(pi), expression(frac(3*pi,2))))

  }
  return(scale)
}


# Set polar coordinates
# ... passed to scale_x_circular to set the template
# @importFrom ggplot2 coord_polar
# @export
polar <- function(...) {
  list(coord_polar(theta="x"), scale_x_circular(...))
}


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

