\name{pause}
\alias{pause}
\title{Pause the experiment.}
\usage{
  pause(experiment)
}
\arguments{
  \item{experiment}{Object of class Experiment}
}
\value{
  TRUE or FALSE, invisibly
}
\description{
  Experiments can be in status Stopped, Waiting, Started,
  or Paused. \code{pause(experiment)} moves the experiment
  from Started to Paused. Clients will be shown a waiting
  page until the experiment is continued.
}
\seealso{
  Other command.line.functions: \code{\link{info}},
  \code{\link{map}}, \code{\link{next_period}},
  \code{\link{print}}, \code{\link{ready}},
  \code{\link{restart}}, \code{\link{start}}
}

