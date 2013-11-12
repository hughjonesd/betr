\docType{data}
\name{stage}
\alias{NEXT}
\alias{stage}
\alias{WAIT}
\title{Create a stage for an experiment}
\format{num -1}
\usage{
  stage(handler)

  NEXT

  WAIT
}
\arguments{
  \item{handler}{A function which returns either a
  character string containing HTML, or the constant
  \code{WAIT}, or the constant \code{NEXT}.}
}
\value{
  a Stage object suitable for adding to an experiment.
}
\description{
  Stages are the building blocks of experiments. A single
  stage can result in one or more HTML pages shown to
  participants.
}
\details{
  If \code{WAIT} is returned, the participant will be shown
  a standard "waiting page" which refreshes after a defined
  time interval. If \code{NEXT} is returned, the
  participant will be moved forward to the next period, and
  the next stage will be called immediately.
}
\examples{
stg <- stage(function(id, period, params) {
 if(params$done=="OK") return(NEXT)
 paste0("<html><body><p>Your ID is ", id, " and the period is", period,
       "</p><form action='", self_url, "' method=POST>
       <input type='Submit' name='done' value='OK'></form>")
})
}
\seealso{
  Other stages: \code{\link{structured_stage}},
  \code{\link{text_stage}}
}
\keyword{datasets}

