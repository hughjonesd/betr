\name{add_stage}
\alias{add_stage}
\title{Add a stage to an experiment}
\usage{
  add_stage(experiment, ..., times, each, after)
}
\arguments{
  \item{experiment}{an Experiment object}

  \item{...}{one or more Stage objects}

  \item{times}{how many times to repeat the sequence of
  stages in \code{...}.  All stages are repeated if this is
  a single number; if it is a vector it gives how many
  times to repeat each stage.}

  \item{each}{how many times to repeat each individual
  stage}

  \item{after}{Add stages after what period}
}
\description{
  Add a stage to an experiment
}
\examples{
expt <- experiment(N=1, autostart=TRUE)
s1 <- stage(function(id, period, params) return("Got to s1!)
s2 <- stage(function(id, period, params) return("Got to s2!)
add_stage(expt, s1, s2, times=2) # s1 s2 s1 s2
add_stage(expt, s1, s2, times=1:2) # s1 s2 s2
add_stage(expt, s1, s2, each=2) # s1 s1 s2 s2
}

