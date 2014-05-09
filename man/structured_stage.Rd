\name{structured_stage}
\alias{structured_stage}
\title{Create a stage in a standardized format.}
\usage{
  structured_stage(form, result, timeout=NULL,
    on_timeout=NULL, process=NULL, wait_for=NULL)
}
\arguments{
  \item{form}{a function, file or character vector. See
  below.}

  \item{result}{a function, file or character vector. See
  below.}

  \item{timeout}{a number of seconds to wait for user input
  from \code{form}.  After \code{timeout} seconds the page
  will be refreshed automatically and the stage will call
  \code{on_timeout}. If \code{NULL} (the default), there is
  no timeout.}

  \item{on_timeout}{a function taking two arguments, like
  \code{function(id, period)}.  If this function returns a
  \code{list}, the list will be assigned as request
  parameters, as if they had been come from the subject.
  Ignored if \code{NULL}.}

  \item{process}{a function taking three arguments, like
  \code{function(id, period, params)}. If it calls
  \code{stop} with an error, the error message will be
  passed back to \code{form}. Ignored if \code{NULL}.}

  \item{wait_for}{either a function, or a list of vectors
  of IDs. See below.  If \code{NULL}, then participants
  move immediately to results.}
}
\value{
  An object of class StructuredStage.
}
\description{
  Structured stages contain a \code{form} (which asks some
  questions of the participants), and a \code{result},
  which does something with the data and optionally shows
  participants the results. They can also optionally
  process the form output and check for errors, timeout
  after a number of seconds, and wait for some participants
  to finish before proceeding. Gory details below.
}
\details{
  Structured stages use the following flowchart.

  1. Has the participant been marked as \emph{finished}?

  Yes => return \code{NEXT}. No => continue:

  2. Has the participant been marked as \emph{ready}?

  Yes => go to 3. No => go to 5.

  3. Are all participants in \code{wait_for} (see below)
  ready?

  No => return \code{WAIT}. Yes => continue:

  4. Mark the participant as \emph{finished}. Return the
  \code{results} section, which returns some HTML or
  \code{NEXT}.

  5. Has the participant seen the stage already in this
  period?

  No => call the stage's \code{form} function, which
  returns some HTML, and start counting \code{timeout} from
  now.

  Yes => continue:

  6. Did the participant respond within \code{timeout}
  seconds?

  No => call the stage's \code{on_timeout} function, then
  continue:

  Yes => continue:

  7. Call the \code{process} function.

  If this \code{stop}s => call \code{form} again, passing
  the error message. The timeout is \emph{not} reset.

  Otherwise => mark the participant as \emph{ready} and
  return to stage 1.

  \code{form} and \code{results} may be connections
  resulting from a call to \code{\link{file}}, character
  vectors, or functions which return HTML. Files will be
  opened and returned. If the file name ends in
  \code{.brew} then it will be processed by
  \code{\link{brew}}. Character vectors are returned as-is.

  If \code{form} is a function, it should take four
  arguments, as follows: \code{function(id, period, params,
  error)}.

  \code{params} and \code{error} may both be missing.

  If \code{result} is a function, it should take three
  arguments, like \code{function(id, period, params)}.

  \code{params} may be missing.

  If \code{wait_for} is a function, it should be of the
  form \code{function(id, period, params, ready)}.

  \code{ready} is a vector containing the ids of
  participants who have been marked as ready. The
  participant will move on to \code{results} only when the
  function returns \code{TRUE}. If \code{wait_for} is a
  list of vectors, then the participant will move on only
  when all participants in the same vector are ready. For
  example, \code{wait_for=list(1:4,5:8,9:12,13:16)}
  requires that participant IDs 1 to 4 are all ready before
  participant 1 can move to \code{results}.
}
\examples{
s1 <- structured_stage(
  form = function(id, period, params, error) {
    c("<html><body>", if(nchar(error)) c("<p style='color:red'>", error,
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
  Other stages: \code{\link{NEXT}}, \code{\link{WAIT}},
  \code{\link{stage}}, \code{\link{text_stage}}
}

