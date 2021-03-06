% Generated by roxygen2 (4.0.2): do not edit by hand
\name{period}
\alias{period}
\title{Begin a new period, optionally waiting for other subjects}
\usage{
period(wait_for="none")
}
\arguments{
\item{wait_for}{}

\item{name}{Optional name of the stage}
}
\value{
An object of class Period
}
\description{
A period may contain one or more stages. When the experiment is started,
all subjects are in period 1. Typically, you will want a new period for each
"repetition" of an experiment. You can then store the data in a data frame
which looks like:
}
\details{
\tabular{lll}{
ID \tab period \tab ...  \cr
1 \tab 1 \tab ... \cr
1 \tab 2 \tab ... \cr
1 \tab ... \tab ... \cr
2 \tab 1 \tab ... \cr
... \tab ... \tab ... \cr
}

I\code{wait_for} is interpreted just as in \code{\link{checkpoint}}.

If all relevant subjects are ready, the subject's period counter is
incremented and the subject moves on.
}
\examples{
expt <- experiment(N=4)
groups <- c("A", "A", "B", "B")
s1 <- text_stage(page="<html><body><form action=''>
     <input type='submit' value='Next'></form></body></html>")

# go ahead individually:
add_stage(expt, period(), s1)

# wait for everyone:
add_stage(expt, period("all"), s1)

# players 1 and 2 wait for each other, so do 3 and 4:
add_stage(expt, period(groups), s1)
}
\seealso{
Other stages: \code{\link{NEXT}}, \code{\link{WAIT}},
  \code{\link{stage}}; \code{\link{checkpoint}};
  \code{\link{form_stage}}; \code{\link{program}};
  \code{\link{text_stage}}; \code{\link{timed}}
}

