\name{restart}
\alias{restart}
\title{Restart the experiment after pausing}
\usage{
  restart(experiment)
}
\arguments{
  \item{experiment}{Object of class Experiment}
}
\value{
  TRUE or FALSE, invisibly
}
\description{
  Experiments can be in status Stopped, Waiting, Started,
  or Paused. \code{restart(experiment)} moves the
  experiment from Paused back to Started.
}
\seealso{
  Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{next_period}}, \code{\link{pause}},
  \code{\link{print}}, \code{\link{ready}},
  \code{\link{replay}}, \code{\link{session_name}},
  \code{\link{start}}
}

