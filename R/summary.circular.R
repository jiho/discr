#' Descriptive statistics and Rayleigh test
#'
#' @param object vector of angles, of class \code{circular}
#' @param ... passthrough argument
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
summary.circular <- function(object, ...) {

  # check the class of angles
  if ( ! is.circular(object) ) {
    stop("object needs to be of class circular")
  }

  # sample size
  n <- length(object)

  # mean angle
  mean <- mean.circular(object)

  # rayleigh test
  rayleigh <- rayleigh.test(object)
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
