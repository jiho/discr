# Compute the subsampling lag to achieve a given subsample in seconds
#
# @param x vector of almost equally spaced date and times (class POSIXct)
# @param sub subsampling interval in seconds
#
# @examples
# subsample_n(Sys.time() + seq(0, 10, by=2), sub=4)
# subsample_n(Sys.time() + seq(0, 10, by=3), sub=4, verbose=TRUE)
#
# subsample_n(Sys.time() + jitter(seq(0, 10, by=2), amount=0.1), sub=4, verbose=TRUE)
# \donttest{subsample_n(Sys.time() + jitter(seq(0, 10, by=3), amount=3), sub=4, verbose=TRUE)}
#' @importFrom lubridate is.POSIXt
subsample_n <- function(x, sub, verbose=FALSE, ...) {

  # compute the subsampling rate
  if ( is.null(sub) ) {
    subN <- 1
  } else {
    # checks
    if (!is.POSIXt(x)) {
      stop("Need a vector of date and times of class POSIXct")
    }
    
    intervals <- diff(as.numeric(x))
    if ( diff(range(intervals)) > sub ) {
      stop("The sequence of times in x is too irregular")
    }

    # compute the actual mean interval between records in seconds
    interval <- mean(intervals)

    # compute the subsampling interval
    # one data every subN will give an interval of sub seconds, approximately
    # NB: we need this to be at least 1
    subN <- max(round(sub / interval), 1)
    
    if ( verbose ) {
      # print the actual subsampling interval
      selected <- x[seq(1, length(x), by=subN)]
      actualIntervals <- as.numeric(diff(selected))
      disc_message("subsample every ", round(mean(actualIntervals), 2), " +/- ", round(sd(actualIntervals), 2)," seconds")
    }
  }

  return(subN)
}