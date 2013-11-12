\name{print}
\alias{info}
\alias{map}
\alias{print}
\alias{print,Experiment,ANY-method}
\alias{show,Experiment,ANY-method}
\title{Show basic info about an experiment}
\usage{
  info(experiment, subj = TRUE, map = TRUE)

  map(experiment)
}
\arguments{
  \item{subj}{if TRUE, print the subjects table}

  \item{map}{if TRUE, also calls \code{map}}
}
\description{
  \code{info} prints information about the status,
  including session name, number of stages, number of
  clients connected and total N, status (Stopped, Waiting,
  Started or Paused) and the URL where the experiment is
  serving. \code{map} shows a map of how subjects are
  progressing through the stages.
}
\examples{
expt <- experiment(N=2, port=12345)
expt # on the command line, calls info()
}
\seealso{
  Other command line functions: \code{\link{next_period}},
  \code{\link{pause}}, \code{\link{ready}},
  \code{\link{restart}}, \code{\link{start}}
}

