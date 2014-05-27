as.heading <- function(x) {
  if ( ! is.circular(x) ) {
    # cast to circular type when not circular, assuming the angles are indeed following the proper conventions
    x <- circular(x, units="degrees", template="geographics", modulo="2pi")
  } else {
    # convert a circular-classed angle to the geographic conventions
    x <- conversion.circular(x, units="degrees", template="geographics", modulo="2pi")
  }
  return(x)
}

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

# Convert object x of class circular (or in trigonometric reference) to bearings
#
# Bearings are measured clockwise from the vertical in degrees
# Trigonometric angles are measured counterclockwise from the horizontal in radians
#' @import circular
trig2geo <- function(x) {
	# Cast to circular type
	if (!is.circular(x)) {
		x <- circular(x)
	}
	# Proceed to the conversion
	x <- conversion.circular(x, units="degrees", template="geographics", modulo="2pi")
	return(x)
}

#
#	Convert object x of class circular (or containing bearings) to trigonometric angles
#
#	Bearings are measured clockwise from the vertical in degrees
#	Trigonometric angles are measured counterclockwise from the horizontal in radians
#
#' @import circular
geo2trig <- function(x) {

	# Cast to circular type
	if (!is.circular(x)) {
		x <- circular(x, units="degrees", template="geographics", modulo="2pi")
	}
	# Proceed to the conversion
	x <- conversion.circular(x, units="radians", template="none", modulo="2pi", zero=0, rotation="counter")
	return(x)
}

#
# Translates from cardinal to polar coordinates
#	incar		matrix or data frame with columns [x,y]
#	orig		vector with the x,y coordinates of the origin
# Result: a matrix or data.frame with columns [theta,rho], with theta in radians and rho in the same unit as the input x and y
#
#' @import circular
car2pol <- function (incar, orig=c(0,0)) {
	# Makes the coordinates relative to the origin
	origMat <- incar
	origMat[,1] <- orig[1]
	origMat[,2] <- orig[2]
	incar <- incar - origMat

	# Initiate inpol
	inpol <- incar
	inpol[,] <- NA

	# Calculate the angles
	inpol[,1] <- atan2(incar[,2], incar[,1])
	inpol[,1] <- ( inpol[,1] + 2*pi ) %% ( 2*pi )
	# Calculate the norms
	inpol[,2] <- sqrt(incar[,1]^2 + incar[,2]^2)

	# Change column names
	names(inpol) <- c("theta","rho")

	# Convert to class circular
	inpol$theta <- circular(inpol$theta)

	return(inpol)
}

#
# Translates from polar to cardinal coordinates
#	inpol		a matrix or data.frame with columns [theta,rho], with theta of class circular or in trigonometric reference
#	orig		vector with the x,y coordinates of the origin
# Result: a matrix or data.frame with columns [x,y] in the same unit as rho
#
#' @import circular
pol2car <- function (inpol, orig=c(0,0)) {
	# Initiate incar
	incar <- inpol
	incar[,] <- NA

	# Make sure angles are in the right reference
	inpol[,1] <- as.trig(inpol[,1])

	# Compute cartesian coordinates
	incar[,1] <- inpol[,2]*cos(inpol[,1])
	incar[,2] <- inpol[,2]*sin(inpol[,1])

	# Make the coordinates relative to the origin
	origMat <- incar
	origMat[,1] <- orig[1]
	origMat[,2] <- orig[2]
	incar <- incar + origMat

	# Change column names
	names(incar) <- c("x","y")

	return(incar)
}

#
# "Linearly" interpolates angles along a circle
#	x			"coordinates" (e.g. time of measurement) of the angles to be interpolated
#	angles	angles to be interpolated, of class circular or in trigonometric reference
#	xout		"coordinates" where the interpolation should take place
#	...		passed to approx
#
#' @import circular
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
