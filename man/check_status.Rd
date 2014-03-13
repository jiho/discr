\name{check_status}
\alias{check_status}
\title{Check the exit status of a command (typically run through \code{system})}
\usage{
check_status(status, message = "discuss error or unexpected termination", ...)
}
\arguments{
  \item{status}{status code, usually an integer}

  \item{message}{a character string with the message to
  print}

  \item{...}{passed to \code{stop}}
}
\description{
Check the exit status of a command (typically run through
\code{system})
}
\keyword{internal}

