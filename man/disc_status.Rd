\name{disc_status}
\alias{disc_status}
\alias{dstatus}
\title{Status of the working directory}
\usage{
disc_status(dir = disc_getwd())

dstatus(dir = disc_getwd())
}
\arguments{
  \item{dir}{path to the working directory. Read from the
  option \code{disc.wd} by default.}
}
\description{
Give information about deployments in the working directory
(number of pictures, metadata files, data files, etc.)
}
\seealso{
\code{\link{disc_setwd}} to set the working directory
option.
}

