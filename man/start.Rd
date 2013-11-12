\name{start}
\alias{start}
\title{Start the experiment running.}
\usage{
  start(experiment, force = FALSE)
}
\arguments{
  \item{experiment}{Object of class Experiment}

  \item{force}{Start the experiment even if there are fewer
  than N participants}
}
\value{
  TRUE or FALSE, invisibly
}
\description{
  Experiments can be in status Stopped, Waiting, Started,
  or Paused. \code{start} moves the experiment from Waiting
  to Started. If the experiment has autostart set, this
  will happen automatically when N subjects have connected.
  When the experiment starts, all subjects are moved to the
  first stage.
}
\seealso{
  Other command line functions: \code{\link{info}},
  \code{\link{map}}, \code{\link{next_period}},
  \code{\link{pause}}, \code{\link{print}},
  \code{\link{ready}}, \code{\link{restart}}
}

