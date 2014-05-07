\name{next_period}
\alias{next_period}
\title{Move some clients forward one period}
\usage{
  next_period(experiment, subjid)
}
\arguments{
  \item{experiment}{Object of class Experiment}

  \item{subjid}{numeric vector of subject id(s) to move
  forward, or data frame from subjects table}
}
\value{
  TRUE or FALSE, invisibly
}
\description{
  Manually moves one or more clients forward. This may
  break your experimental design so use in emergencies
  only!
}
\seealso{
  Other command line functions: \code{\link{get_url}},
  \code{\link{info}}, \code{\link{map}},
  \code{\link{pause}}, \code{\link{print}},
  \code{\link{ready}}, \code{\link{replay}},
  \code{\link{restart}}, \code{\link{session_name}},
  \code{\link{start}}
}

