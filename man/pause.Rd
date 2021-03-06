% Generated by roxygen2 (4.0.2): do not edit by hand
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
Experiments can be in status Stopped, Waiting, Started, or Paused.
\code{pause(experiment)} moves the experiment from Started to Paused.
Clients will be shown a waiting page until the experiment is continued.
}
\seealso{
Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{nperiods}},
  \code{\link{print,Experiment,ANY-method}},
  \code{\link{print,Experiment-method}},
  \code{\link{print_stages}}, \code{\link{session_name}};
  \code{\link{halt}}; \code{\link{load_commands}};
  \code{\link{merge_subjects}}; \code{\link{next_stage}};
  \code{\link{ready}}; \code{\link{replay}};
  \code{\link{restart}}; \code{\link{start.Experiment}};
  \code{\link{trace_stage}}, \code{\link{untrace_stage}}
}

