% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{data}
\name{stage}
\alias{NEXT}
\alias{WAIT}
\alias{stage}
\title{Create a stage for an experiment}
\format{\preformatted{Class 'NEXT'  num -1
}}
\usage{
stage(handler, name = "No name")

NEXT

WAIT
}
\arguments{
\item{handler}{A function which returns either a character string
containing HTML, a \code{\link{Rook::Response}} object, the constant
\code{WAIT}, or the constant \code{NEXT}.}

\item{name}{optional name of the stage.}
}
\value{
a Stage object suitable for adding to an experiment.
}
\description{
Stages are the building blocks of experiments. A single stage can
result in one or more HTML pages shown to participants.
}
\details{
If \code{WAIT} is returned, the participant will be shown
         a standard "waiting page" which refreshes after a defined
         time interval. If HTML or a \code{Response} object is returned,
         then it is passed back to the participant. In these cases the stage
         will be called again next time the participant makes a request. If
         \code{NEXT} is returned, the participant will be moved forward to
         the next period, and the next stage will be called immediately.
}
\examples{
stg <- stage(function(id, period, params) {
 if(params$done=="OK") return(NEXT)
 c("<html><body><p>Your ID is ", id, " and the period is", period,
       "</p><form action='' method=POST>
       <input type='Submit' name='done' value='OK'></form></body><html>")
})
}
\seealso{
Other stages: \code{\link{checkpoint}};
  \code{\link{form_stage}}; \code{\link{period}};
  \code{\link{program}}; \code{\link{text_stage}};
  \code{\link{timed}}
}
\keyword{datasets}

