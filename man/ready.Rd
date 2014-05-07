\name{ready}
\alias{ready}
\title{Set the experiment up to receive participants.}
\usage{
  ready(experiment)
}
\arguments{
  \item{experiment}{Object of class Experiment}
}
\value{
  TRUE or FALSE, invisibly
}
\description{
  Experiments can be in status Stopped, Waiting, Started,
  or Paused. \code{ready(experiment)} moves the experiment
  from Stopped to Waiting. Clients can now connect and will
  be shown a waiting page.
}
\seealso{
  Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{next_period}}, \code{\link{pause}},
  \code{\link{print}}, \code{\link{replay}},
  \code{\link{restart}}, \code{\link{session_name}},
  \code{\link{start}}
}

