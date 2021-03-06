% Generated by roxygen2 (4.0.2): do not edit by hand
\name{replay}
\alias{replay}
\title{Replay part or all of an experiment}
\usage{
replay(experiment, folder = NULL, maxtime = Inf, speed = NULL,
  ask = FALSE, live = FALSE, clients = NULL)
}
\arguments{
\item{experiment}{an object of class Experiment}

\item{folder}{which record to use. Default is the current session
or the most recent session found}

\item{maxtime}{replay only 'maxtime' seconds of the original session}

\item{speed}{how long to wait between each command or request}

\item{ask}{ask before replaying each command or request}

\item{live}{run the replay "live", with the web server continuing to serve}

\item{clients}{(character vector) only replay requests from \code{clients}}
}
\description{
\code{replay} plays back an experiment using records stored on disk.
All requests from clients, and all commands issued on the command line,
will be replayed. Afterwards the experiment can be continued.
}
\details{
betr records requests and commands in a folder named <experiment
name>-YYYY-MM-DD-HHMMSS, where the date and time record when
\code{\link{ready}} was called. Within this folder, the subfolder 'record'
holds details. If \code{folder} is not given, the default is either the
current session name, or the most recently accessed folder matching the
format above.

Each file in the folder records a single command or request. The filename
records the time after experiment start that the command/request was processed.
Details are stored in the file using the YAML format, which is quite
self-explanatory and easy to edit.

Unless \code{live=TRUE}, the experiment will be stopped before replay(if it is already started). In any case, the
subject table will be reinitialized and \code{\link{ready}} will be called.
This has the effect of calling any user-defined \code{on_ready} function.

\code{speed} can be numeric or "realtime". A numeric gives the number of
seconds to wait before executing each command or request. "realtime" means,
go at the speed of the original session. \code{live=TRUE} implies
\code{speed="realtime"} unless \code{speed} is explicitly set.

If \code{maxtime} is given, only the first \code{maxtime} seconds of the experiment will be replayed.
This is useful if you want to "rewind" the experiment because of lab problems,
or during debugging.

If \code{ask} is \code{TRUE} then the experimenter will be prompted before each
command or request is replayed:

\itemize{
  \item enter or 'n' replays the command/request
  \item 'c' continues replaying to the end, without prompting again
  \item 's' skips the command/request
  \item 'q' skips this and all other commands/requests
  \item 'd' shows details of the command/request
  \item '?' or 'h' shows a help message
  \item Any other item will be evaluated in the global environment
}

Note that replay creates a new session. This means that you cannot do

\code{
replay(expt, maxtime=30)
replay(expt, maxtime=120)
}

: the first replay will have created a new session with only the commands
from the first 30 seconds. If you want to move backward and forward
within a session, use \code{replay(expt, folder="xxx")} where xxx is the specific
session of interest.
}
\examples{
start(expt)
# something goes wrong after 2 minutes
replay(expt, maxTime=120)
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
  \code{\link{pause}}; \code{\link{ready}};
  \code{\link{restart}}; \code{\link{start.Experiment}};
  \code{\link{trace_stage}}, \code{\link{untrace_stage}}
}

