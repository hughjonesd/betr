#' @import brew

#' @export AbstractStage
#' @exportClass AbstractStage
AbstractStage <- setRefClass("AbstractStage",
  fields=list(expt="ANY"),
  methods=list(
    initialize = function(...) callSuper(...),
    
    handle_request = function(id, period, params) stop(
          "handle_request called on object of class AbstractStage")
  )
)

#' @export Stage
#' @exportClass Stage
Stage <- setRefClass("Stage", contains="AbstractStage",
  fields=list(
    .handle_request = "function"
  ),
  methods=list(
    initialize = function(handler, ...) {
      if (! missing(handler)) .handle_request <<- handler
      callSuper(...)
    },
    
    handle_request = function(id, period, params) {
      .handle_request(id, period, params)
    }
  )
)

#' Create a stage for an experiment
#' 
#' Stages are the building blocks of experiments. A single stage can
#' result in one or more HTML pages shown to participants.
#' @param handler A function which returns either a character string 
#'        containing HTML, a \code{\link{Rook::Response}} object, the constant 
#'        \code{WAIT}, or the constant \code{NEXT}.
#' @return a Stage object suitable for adding to an experiment.
#' @details If \code{WAIT} is returned, the participant will be shown
#'          a standard "waiting page" which refreshes after a defined
#'          time interval. If HTML or a \code{Response} object is returned, 
#'          then it is passed back to the participant. In these cases the stage 
#'          will be called again next time the participant makes a request. If 
#'          \code{NEXT} is returned, the participant will be moved forward to 
#'          the next period, and the next stage will be called immediately.
#' @examples
#' stg <- stage(function(id, period, params) {
#'  if(params$done=="OK") return(NEXT)
#'  c("<html><body><p>Your ID is ", id, " and the period is", period,
#'        "</p><form action='' method=POST>
#'        <input type='Submit' name='done' value='OK'></form></body><html>")
#' })
#' @family stages
#' @export
stage <- function (handler) Stage$new(handler)


#' @export TextStage
#' @exportClass TextStage
TextStage <- setRefClass("TextStage", contains="AbstractStage",
  fields=list(
    text="character",
    file="character",
    shown="numeric"
  ),
  methods=list(
    initialize = function(text, file, ...) {
      if (! missing(text) && ! missing(file)) 
            stop("Only one of body or file should be defined")
      if (! missing(text)) text <<- text
      if (! missing(file)) file <<- file
      callSuper(shown=numeric(0), ...)
    },
    
    handle_request = function(id, period, params) {
      if (id %in% shown) return(NEXT)
      shown <<- c(shown, id)
      if (length(text)>0) return(text)
      # don't crash if a file is missing?
      tryCatch(html <- file_or_brew(file), error= function(e) warning(e))
      return(html)
    }
  )
)

#' Create a stage which does nothing but display HTML to the subject.
#' 
#' A text stage presents some HTML once to each subject.
#' 
#' @param file A filepath. If the file ends in ".brew" then it is passed to 
#'        \code{brew} for processing. Otherwise it is shown to the subject as-is.
#' @param text A character vector of HTML. Only one of \code{file} and
#'        \code{text} should be passed.
#' @return An object of class TextStage. When called the first time, this will
#'         display the HTML in \code{file} or \code{text} to the participant. 
#'         Subsequent calls will return \code{NEXT}. 
#' @details It is always safe to call \code{next_stage} on a participant who
#'          is at a TextStage.
#' @family stages  
#' @export
text_stage <- function (...) TextStage$new(...)

file_or_brew <- function(fb, error=NULL) {
  fbn <- if (class(fb)=="file") summary(fb)$description else fb
  error <- error
  # WTF does brew() not just return a string?
  if (grepl("\\.brew$", fbn)) {
    capture.output(brew(fbn))
  } else {
    readLines(fb)
  }    
}

ffbc <- function(thing, ..., error=NULL) {
  if (is.function(thing)) return(if (is.null(error)) thing(...) else thing(..., error)) # ugly
  if (inherits(thing, "file")) return(file_or_brew(thing, error))
  if (is.character(thing)) return(thing)
  stop("Unrecognized object of class ", class(thing), "passed to ffbc")
}

rookify <- function (thing) {
  if (inherits(thing, "Response")) return(thing)
  rr <- Rook::Response$new()
  rr$write(thing)
  return(rr)
}

#' @export StructuredStage
#' @exportClass StructuredStage
StructuredStage <- setRefClass("StructuredStage", contains="AbstractStage",
  fields = list(
    form        = "ANY",
    timeout     = "ANY",
    on_timeout  = "ANY",
    process     = "ANY",
    wait_for    = "ANY",
    result      = "ANY",
    started     = "numeric",
    ready       = "numeric",
    finished    = "numeric",
    timestamps  = "list"
  ),
  methods = list(
    initialize = function (timeout=NULL, on_timeout=NULL, wait_for=NULL, 
          process=NULL, ...) {
      callSuper(timeout=timeout, on_timeout=on_timeout, wait_for=wait_for,
            process=process, ...)
    },
    
    handle_request = function (id, period, params) {    
      if (id %in% finished) return(NEXT)
      
      if (id %in% ready) {
        # can we move to result?
        if (is.null(wait_for)) {
          do_result <- TRUE
        } else if (is.function(wait_for)) {
          do_result <- wait_for(id, period, params, ready) 
        } else {
          ids <- unlist(wait_for[sapply(wait_for, '%in%', x=id)])
          do_result <- all(ids %in% ready)
        }
        if (do_result) {
          # if so, do it and mark id as finished
          output <- ffbc(result, id, period, params)
          finished <<- c(finished, id)
          return(output)
        } else {
          # otherwise wait
          return(WAIT)
        }
      }
      
      # we are not ready. 
      if (! id %in% started) {
        output <- ffbc(form, id, period, params, error=NULL)
        if (is.next(output) || is.wait(output)) return(output) 
        rr <- rookify(output)
        if (! is.null(timeout)) {
          timestamps[[id]] <<- Sys.time()
          rr$header("Refresh", timeout)
        }
        started <<- c(started, id)
        return(rr)
      } else {
        if (! is.null(timeout) && Sys.time() > timestamps[[id]] + timeout) {
        # if we've timed out, run on_timeout
          maybe <- NULL
          if(is.function(on_timeout)) {
            maybe <- on_timeout(id, period)
          } 
          if (is.list(maybe)) params <- maybe
        }
        # process the result, if needed
        if (is.function(process)) {
          chk <- tryCatch(process(id, period, params), error= function(e) e)
          if (inherits(chk, "error")) {
            return(ffbc(form, id, period, params, error=chk$message))
          }
        }
        # mark the participant as ready and call again from the top
        ready <<- c(ready, id)
        return(handle_request(id, period, params))
      }
    }
  )
)

#' Create a stage in a standardized format.  
#' 
#' Structured stages contain a \code{form} (which asks some questions of the 
#' participants), and a \code{result}, which does something with the data and optionally
#' shows participants the results. They can also optionally process the form
#' output and check for errors, timeout after a number of seconds, and wait for
#' some participants to finish before proceeding. Gory details below.
#' 
#' @usage structured_stage(form, result, timeout=NULL, on_timeout=NULL, process=NULL, 
#'        wait_for=NULL)
#' @param form a function, file or character vector. See below.
#' @param result a function, file or character vector. See below.
#' @param timeout a number of seconds to wait for user input from \code{form}.
#'        After \code{timeout} seconds the page will be refreshed automatically
#'        and the stage will call \code{on_timeout}. If \code{NULL} (the default),
#'        there is no timeout.
#' @param on_timeout a function taking two arguments, like \code{function(id, period)}.
#'        If this function returns a \code{list}, the list will be assigned as 
#'        request parameters, as if they had been come from the subject. 
#'        Ignored if \code{NULL}.
#' @param process a function taking three arguments, like 
#'        \code{function(id, period, params)}. If it calls \code{stop} with an 
#'        error, the error message will be passed back to \code{form}. Ignored
#'        if \code{NULL}.
#' @param wait_for either a function, or a list of vectors of IDs. See below. 
#'        If \code{NULL}, then participants move immediately to results.
#' 
#' @details 
#' Structured stages use the following flowchart.
#' 
#' 1. Has the participant been marked as \emph{finished}?
#' 
#' Yes => return \code{NEXT}. No => continue:
#' 
#' 2. Has the participant been marked as \emph{ready}?
#' 
#' Yes => go to 3. No => go to 5.
#' 
#' 3. Are all participants in \code{wait_for} (see below) ready?
#' 
#' No => return \code{WAIT}. Yes => continue:
#' 
#' 4. Mark the participant as \emph{finished}. Return the \code{results} section, 
#' which returns some HTML or \code{NEXT}.
#' 
#' 5. Has the participant seen the stage already in this period?
#' 
#' No => call the stage's \code{form} function, which returns some HTML,
#'       and start counting \code{timeout} from now. 
#'       
#' Yes => continue: 
#' 
#' 6. Did the participant respond within \code{timeout} seconds?
#' 
#' No => call the stage's \code{on_timeout} function, then continue:
#' 
#' Yes => continue:
#' 
#' 7. Call the \code{process} function. 
#' 
#' If this \code{stop}s => call \code{form} again, passing the error message.
#' The timeout is \emph{not} reset.
#' 
#' Otherwise => mark the participant as \emph{ready} and return to stage 1.
#' 
#' \code{form} and \code{results} may be connections resulting from a call to 
#' \code{\link{file}}, character vectors, or functions which return HTML.
#' Files will be opened and returned. If the file name ends in \code{.brew} then
#' it will be processed by \code{\link{brew}}. Character vectors are returned 
#' as-is.
#' 
#'  If \code{form} is a function, it should take four arguments, as follows: 
#'        \code{function(id, period, params, error)}. 
#'        
#'  \code{params} and \code{error} may both be missing. 
#'  
#'  If \code{result} is a function, it should take three arguments, like 
#'        \code{function(id, period, params)}. 
#'        
#'  \code{params} may be missing.
#'  
#' If \code{wait_for} is a function, it should be of the form 
#'        \code{function(id, period, params, ready)}. 
#'        
#'        \code{ready} is a vector containing the ids of participants who have been 
#'        marked as ready. The participant will
#'        move on to \code{results} only when the function returns \code{TRUE}. If
#'        \code{wait_for} is a list of vectors, then the participant will move on only when 
#'        all participants in the same vector are ready. For example, 
#'        \code{wait_for=list(1:4,5:8,9:12,13:16)} requires that participant
#'        IDs 1 to 4 are all ready before participant 1 can move to \code{results}.
#' @examples
#' s1 <- structured_stage(
#'   form = function(id, period, params, error) {
#'     c("<html><body>", if(nchar(error)) c("<p style='color:red'>", error, 
#'     "</p>"), "<form action='' method=POST>Enter your name:<input type='text'
#'     name='name'></form></body></html>")
#'   },
#'   process = function(id, period, params) {
#'     if (! 'name' %in% params || nchar(params$name) == 0) {
#'       stop("Please enter your name!")
#'     }
#'     pnames[[id]] <<- params$name
#'   },
#'   wait_for = list(1:4, 5:8, 9:12, 13:16),
#'   result = function(id, period, params) {
#'     ids <- list(1:4, 5:8, 9:12, 13:16)
#'     myids <- unlist(ids[sapply(ids, '%in%', x=id)])
#'     c("<html><body><p>Say hello to each other:</p><p>", 
#'     paste(pnames[ids], "<br>"), "</p></body></html>")
#'   }
#' )
#' @return An object of class StructuredStage.
#' @family stages
#' @export
structured_stage <- function (...) StructuredStage$new(...)


Period <- setRefClass("Period", contains="AbstractStage",
  fields = list(
    wait_for = "ANY",
    ready = "numeric"
  ),
  methods = list(
    initialize = function(wait_for="none") {
     callSuper(wait_for=wait_for, ready=numeric(0)) 
    },
    
    handle_request = function(id, period, params) {
      if (! id %in% ready) ready <<- c(ready, id)
      ids <- if (length(wait_for)==1 && wait_for=="all") 1:expt$N else 
            if (length(wait_for)==1 && wait_for=="none") numeric(0) else 
            which(wait_for==wait_for[id])
      if (all(ids %in% ready)) {
        expt$next_period(id)
        return(NEXT)
      } else return(WAIT)
    }
  )
)

#' Begin a new period, optionally waiting for other subjects
#' 
#' A period may contain one or more stages. When the experiment is started,
#' all subjects are in period 1. Typically, you will want a new period for each
#' "repetition" of an experiment. You can then store the data in a data frame
#' which looks like:
#' 
#' \tabular{lll}{
#' ID \tab period \tab ...  \cr
#' 1 \tab 1 \tab ... \cr
#' 1 \tab 2 \tab ... \cr
#' 1 \tab ... \tab ... \cr
#' 2 \tab 1 \tab ... \cr
#' ... \tab ... \tab ... \cr
#' }
#' 
#' @usage period(wait_for=NULL)
#' @param wait_for 
#' 
#' @details 
#' 
#' If \code{wait_for} is a vector, then it is assumed to represent
#' subject groups. So, if \code{wait_for} is \code{vec} then a subject with 
#' ID \code{x} will wait here until all subjects with IDs 
#' \code{which(vec==vec[x])} have arrived at this stage. If \code{wait_for} is 
#' \code{NULL} (the default), individual subjects can move on without waiting.
#' 
#' If all relevant subjects are ready, the subject's period counter is
#' incremented and the subject moves on.
#' 
#' @examples
#' expt <- experiment(N=4)
#' groups <- c("A", "A", "B", "B")
#' s1 <- text_stage(text="<html><body><form action=''>
#'      <input type='submit' value='Next'></form></body></html>")
#'      
#' # go ahead individually:
#' add_stage(expt, s1, new_period(), times=2) 
#' 
#' # wait for everyone:
#' add_stage(expt, s1, new_period("all"), times=2) 
#' 
#' # players 1 and 2 wait for each other, so do 3 and 4:
#' add_stage(expt, s1, new_period(groups), times=2) 
#' 
#' @return An object of class NewPeriod
#' @family stages
#' @export
period <- function (...) Period$new(...)


Program <- setRefClass("Program", contains="AbstractStage",
  fields = list(
    fn = "function",
    ready = "numeric",
    run = "character"
  ),
  methods = list(
    initialize = function(fn, run) {
      if (! run %in% c("first", "last", "all")) 
            stop("run must be first, last or all, was ", run)
      callSuper(fn=fn, run=run, ready=numeric(0)) 
    },
    
    handle_request = function(id, period, params) {
      ready <<- c(ready, id)
      if (length(ready) == 1 && run == "first" || run == "all" || 
            length(ready) == expt$N && run ==" last") fn(id, period)    
      return(NEXT)
    }
  )
)


#' Call a function, either once or for each subject
#' 
#' @param fn a function which should take two arguments, \code{id} and 
#' \code{period}.
#' @param run "first", "last", or "all".
#' 
#' @details 
#' If
#' \code{run} is \code{"all"} then the function \code{fn}will be run every time a
#' subject reaches the stage. If \code{run} is \code{"first"} then \code{fn}
#' will be run when the first subject reaches the stage. If \code{run} is 
#' \code{"last"} then \code{fn} will be run when the last subject reaches
#' the stage.
#' 
#' @examples
#' expt <- experiment(N=4)
#' s1 <- text_stage(text="<html><body><form action=''>
#'      Enter a contribution:<input name='contrib' type='text'>
#'      <input type='submit' value='Next'></form></body></html>")
#'      
#' s2 <- program      
#'      
#' @return A Stage object of class Program
#' @family stages
#' @export
program <- function (fn, run) Calculation$new(fn, run) 


#' @rdname stage
#' @export
NEXT <- -1
class(NEXT) <- "NEXT"
#' @rdname stage
#' @export
WAIT <- -2
class(WAIT) <- "WAIT"

is.next <- function(x) inherits(x, "NEXT")
is.wait <- function(x) inherits(x, "WAIT")

