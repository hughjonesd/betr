\name{stage}
\alias{stage}
\title{Create a stage for an experiment}
\usage{
  stage(handler)
}
\arguments{
  \item{handler}{A function which returns either a
  character string containing HTML, or the constant WAIT,
  or the constant NEXT.}
}
\value{
  a Stage object suitable for adding to an experiment.
}
\description{
  Stages are the building blocks of experiments. A single
  stage can result in one or more HTML pages shown to
  participants.
}
\examples{
stg <- stage(function(id, period, params) {
 if(params$done=="OK") return(NEXT)
 paste0("<html><body><p>Your ID is ", id, " and the period is", period,
       "</p><form action='", self_url, "' method=POST>
       <input type='Submit' name='done' value='OK'></form>")
})
}

