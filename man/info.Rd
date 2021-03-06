% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{methods}
\name{print,Experiment-method}
\alias{get_url}
\alias{info}
\alias{map}
\alias{nperiods}
\alias{print,Experiment,ANY-method}
\alias{print,Experiment-method}
\alias{print_stages}
\alias{session_name}
\title{Show basic info about an experiment}
\usage{
experiment

info(experiment, subj = TRUE, map = TRUE)

map(experiment)

get_url(experiment)

session_name(experiment)

nperiods(experiment)

print_stages(experiment)
}
\arguments{
\item{experiment}{an object of class Experiment}

\item{subj}{if TRUE, print the subjects table}

\item{map}{if TRUE, also calls \code{map}}
}
\description{
\code{info} prints information about the status, including session name,
number of stages, number of clients connected and total N,
status (Stopped, Waiting, Started or Paused)
and the URL where the experiment is serving.
\code{map} shows a map of how subjects are progressing through the stages.
\code{get_url} returns the experiment url.
\code{nperiods} returns the number of \code{\link{period}}s in the experiment.
\code{session_name} returns the experiment session name,
or NA if the experiment status is Stopped.
\code{print_stages} prints a list of the stages in the experiment.
}
\examples{
expt <- experiment(N=2, port=12345)
expt # on the command line, calls info()
}
\seealso{
Other command line functions: \code{\link{halt}};
  \code{\link{load_commands}};
  \code{\link{merge_subjects}}; \code{\link{next_stage}};
  \code{\link{pause}}; \code{\link{ready}};
  \code{\link{replay}}; \code{\link{restart}};
  \code{\link{start.Experiment}};
  \code{\link{trace_stage}}, \code{\link{untrace_stage}}
}

