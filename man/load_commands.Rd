% Generated by roxygen2 (4.0.2): do not edit by hand
\name{load_commands}
\alias{load_commands}
\title{Set up simple one-word commands}
\usage{
load_commands(expt)
}
\description{
After \code{load_commands(expt)} is called, entering \code{READY},
\code{START}, \code{HALT}, \code{PAUSE}, \code{RESTART}, \code{INFO},
\code{MAP} or \code{WEB_TEST} will call the corresponding command on the
experiment, with no arguments.
}
\details{
\code{load_commands} is implemented using \code{\link{makeActiveBinding}}.
For obvious reasons, don't assign to the values \code{READY} etc. either before
or after this is called!

\code{unload_commands} simply removes the command names from the global
environment.
}
\examples{
expt <- experiment(name="myexpt", N=2, seats_file=NULL, record=FALSE)
info(expt)
load_commands(expt)
INFO
}
\seealso{
Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{nperiods}},
  \code{\link{print,Experiment,ANY-method}},
  \code{\link{print,Experiment-method}},
  \code{\link{print_stages}}, \code{\link{session_name}};
  \code{\link{halt}}; \code{\link{merge_subjects}};
  \code{\link{next_stage}}; \code{\link{pause}};
  \code{\link{ready}}; \code{\link{replay}};
  \code{\link{restart}}; \code{\link{start.Experiment}};
  \code{\link{trace_stage}}, \code{\link{untrace_stage}}
}

