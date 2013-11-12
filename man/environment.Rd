\name{environment}
\alias{environment}
\title{Return an experiment's environment}
\description{
  Return an experiment's environment
}
\details{
  It is a good idea to assign variables within the
  experiment's environment in your source file before
  defining stages. These variables can then be accessed in
  your stage functions and brew files. They can be written
  to using \code{<<-}. Doing this helps make your
  experiment replay-safe.
}
\examples{
expt <- experiment(N=1)
with(environment(expt),
    mydf <- data.frame(id=numeric(0), profit=numeric(0))
)
}

