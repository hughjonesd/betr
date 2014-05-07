\name{replay}
\alias{replay}
\title{Replay part or all of an experiment}
\usage{
  replay(experiment, folder = NULL, maxtime = Inf,
    speed = NULL, ask = FALSE)
}
\arguments{
  \item{experiment}{an object of class Experiment}

  \item{folder}{which record to use. Default is the current
  session or the most recent session found}

  \item{speed}{how long to wait between each command or
  request}

  \item{maxtime}{replay only 'maxtime' seconds of the
  original session}

  \item{ask}{ask before replaying each command or request}
}
\description{
  \code{replay} plays back an experiment using records
  stored on disk. If the experiment is already started, it
  will be stopped and all information in the experiment's
  environment will be deleted in preparation for the
  replay. All requests from clients, and all commands
  issued on the command line, will be replayed. Afterwards
  the experiment can be continued.
}
\details{
  betr records requests and commands in a folder named
  <experiment name>-YYYY-MM-DD-HHMMSS, where the date and
  time record when \code{\link{ready}} was called. Within
  this folder, the subfolder 'record' holds details. If
  \code{folder} is not given, the default is either the
  current session name, or the most recently accessed
  folder matching the format above.

  \code{speed} can be numeric or "realtime". A numeric
  gives the number of seconds to wait before executing each
  command or request. "realtime" means, go at the speed of
  the original session.

  If \code{maxtime} is given, only the first \code{maxtime}
  seconds of the experiment will be replayed. This is
  useful if you want to "rewind" the experiment because of
  lab problems, or during debugging.

  If \code{ask} is \code{TRUE} then the experimenter will
  be prompted before each command or request is replayed:

  \itemize{ \item 'y' replays the command/request \item 'c'
  continues replaying to the end, without prompting again
  \item 'n' skips the command/request \item 'q' skips this
  and all other commands/requests \item 'd' shows details
  of the command/request \item Any other item will be
  evaluated in the global environment }
}
\examples{
start(expt)
# something goes wrong after 2 minutes
replay(expt, maxTime=120)
}
\seealso{
  Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{next_period}}, \code{\link{pause}},
  \code{\link{print}}, \code{\link{ready}},
  \code{\link{restart}}, \code{\link{session_name}},
  \code{\link{start}}
}

