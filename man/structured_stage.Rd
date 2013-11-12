\name{structured_stage}
\alias{structured_stage}
\title{Create a stage which uses a standardized question/result format.}
\usage{
  structured_stage(...)
}
\arguments{
  \item{form}{A function taking four arguments, like
  \code{function(id, period, params, error)}.
  \code{params} and \code{error} may both be missing. The
  function should return some HTML, or \code{NEXT} if the
  participant should skip this stage entirely.}

  \item{timeout}{A number of seconds to wait for user input
  from \code{form}.  After \code{timeout} seconds the page
  will be refreshed automatically and the stage will call
  \code{on_timeout}. If \code{NULL} (the default), there is
  no timeout.}

  \item{on_timeout}{A function taking two arguments, like
  \code{function(id, period)}.  If it returns a
  \code{list}, the list will be assigned as parameters , as
  if they had been come from the subject. Ignored if
  \code{NULL}.}

  \item{process}{A function taking three arguments, like
  \code{function(id, period, params)}. If it throws an
  error, the error message will be passed back to
  \code{form}. Ignored if \code{NULL}.}

  \item{wait_for}{Either a function, or a list of vectors
  of IDs. See below.  If \code{NULL}, then participants
  move immediately to results.}

  \item{result}{A function taking three arguments, like
  \code{function(id, period, params)}. Should return some
  HTML, or \code{NEXT} if the participant can move directly
  to the next stage.}
}
\value{
  An object of class StructuredStage.
}
\description{
  Structured stages use the following flowchart.
  \preformatted{1. Has the participant finished the stage?
  Yes => return \code{NEXT} No => continue: 2. Has the
  participant seen the stage already in this period? No =>
  call the stage's \code{form} function, which returns some
  HTML, and start counting \code{timeout} from now. Yes =>
  continue: 3. Did the participant respond within
  \code{timeout} seconds? No => call the stage's
  \code{on_timeout} function, then continue to step 5. Yes
  => continue: 4. Call the \code{process} function. If this
  \code{stop}s => call \code{form} again, passing the error
  message.  The timeout is \emph{not} reset. Otherwise =>
  continue: 5. Are all participants in \code{wait_for}
  ready? No => return \code{WAIT}. Yes => continue: 6. Call
  the \code{results} function, which returns some HTML or
  \code{NEXT}. Mark the participant as having finished the
  stage.}
}
\details{
  If \code{wait_for} is a function like \code{function(id,
  period, params)}, then the participant will move on to
  \code{results} only when the function returns
  \code{TRUE}. If it is a list of vectors, then the
  participant will move on only when all participants in
  the same vector are ready. For example,
  \code{wait_for=list(1:4,5:8,9:12,13:16)} requires that
  participant IDs 1 to 4 are all ready before moving to
  \code{results}.
}
\examples{
s1 <- structured_stage(
  form = function(id, period, params, error) {
    c("<html><body>", if(length(error)) c("<p style='color:red'>", error,
    "</p>"), "<form action='' method=POST>Enter your name:<input type='text'
    name='name'></form></body></html>")
  },
  process = function(id, period, params) {
    if (! 'name' \%in\% params || nchar(params$name) == 0) {
      stop("Please enter your name!")
    }
    pnames[[id]] <<- params$name
  },
  wait_for = list(1:4, 5:8, 9:12, 13:16),
  result = function(id, period, params) {
    ids <- list(1:4, 5:8, 9:12, 13:16)
    myids <- unlist(ids[sapply(ids, '\%in\%', x=id)])
    c("<html><body><p>Say hello to each other:</p><p>",
    paste(pnames[ids], "<br>"), "</p></body></html>")
  }
)
}
\seealso{
  Other stages: \code{\link{NEXT}}, \code{\link{stage}},
  \code{\link{text_stage}}, \code{\link{WAIT}}
}

