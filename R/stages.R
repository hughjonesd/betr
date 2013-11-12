
#' @export
AbstractStage <- setRefClass("AbstractStage",
  fields=list(),
  methods=list(
    initialize=function(...) callSuper(...),
    
    handle_request=function(id, period, params) stop(
          "handle_request called on object of class AbstractStage")
  )
)

#' @export
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
    },
    
    .set_handler_env = function(env) {
      environment(.handle_request) <<- env
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

.file_or_brew <- function(fb) {
  # WTF does brew() not just return a string?
  if (grepl("\\.brew$", fb)) capture.output(brew(fb)) else readLines(fb)
}

#' @export
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
      # don't crash if a file is missing
      tryCatch(html <- .file_or_brew(file), error= function(e) warning(e))
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

#' @export
StructuredStage <- setRefClass("StructuredStage", contains="AbstractStage",
  fields = list(
    form        = "function",
    timeout     = "ANY",
    on_timeout  = "ANY",
    process     = "ANY",
    wait_for    = "ANY",
    result      = "function",
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
      if (! id %in% started) {
        html <- form(id, period, params)
        if (! is.character(html)) return(html) # WAIT or NEXT
        if (! is.null(timeout)) {
          timestamps[[id]] <<- Sys.time()
        }
        started <<- c(started, id)
        return(html)
      }
      if (! is.na(timeout) && Sys.time() > timestamps[[id]] + timeout) {
        maybe <-  if(is.function(on_timeout)) on_timeout(id, period) else NULL
        if (is.list(maybe)) params <- maybe
      } else {
        if (is.function(process)) {
          chk <- tryCatch(process(id, period, params), error= function(e) e)
          if (inherits(chk, "error")) return(form(id, period, params))
        }
      }
      ready <<- c(ready, id)
      if (is.null(wait_for)) {
        do_result <- TRUE
      } else if (is.function(wait_for)) {
        do_result <- wait_for(id, period, params) 
      } else {
        ids <- unlist(wait_for[sapply(wait_for, '%in%', x=id)])
        do_result <- all(ids %in% ready)
      }
      if (do_result) {
        html <- result(id, period, params)
        finished <<- c(finished, id)
        return(html)
      }
    }
  )
)

#' Create a stage which uses a standardized question/result format.  
#' 
#' Structured stages use the following flowchart.
#' \preformatted{1. Has the participant finished the stage?
#' Yes => return \code{NEXT}
#' No => continue:
#' 2. Has the participant seen the stage already in this period?
#' No => call the stage's \code{form} function, which returns some HTML,
#'       and start counting \code{timeout} from now.
#' Yes => continue: 
#' 3. Did the participant respond within \code{timeout} seconds?
#' No => call the stage's \code{on_timeout} function, then continue to step 5.
#' Yes => continue:
#' 4. Call the \code{process} function. 
#' If this \code{stop}s => call \code{form} again, passing the error message.
#'                         The timeout is \emph{not} reset.
#' Otherwise => continue:
#' 5. Are all participants in \code{wait_for} ready?
#' No => return \code{WAIT}.
#' Yes => continue:
#' 6. Call the \code{results} function, which returns some HTML or \code{NEXT}.
#' Mark the participant as having finished the stage.}
#' @param form A function taking four arguments, like 
#'        \code{function(id, period, params, error)}. 
#'        \code{params} and \code{error} may both be missing. The function
#'        should return some HTML, or \code{NEXT} if the participant should
#'        skip this stage entirely.
#' @param timeout A number of seconds to wait for user input from \code{form}.
#'        After \code{timeout} seconds the page will be refreshed automatically
#'        and the stage will call \code{on_timeout}. If \code{NULL} (the default),
#'        there is no timeout.
#' @param on_timeout A function taking two arguments, like \code{function(id, period)}.
#'        If it returns a \code{list}, the list will be assigned as parameters 
#'        , as if they had been come from the subject. Ignored if \code{NULL}.
#' @param process A function taking three arguments, like 
#'        \code{function(id, period, params)}. If it throws an error, the error
#'        message will be passed back to \code{form}. Ignored if \code{NULL}.
#' @param wait_for Either a function, or a list of vectors of IDs. See below. 
#'        If \code{NULL}, then participants move immediately to results.
#' @param result A function taking three arguments, like 
#'        \code{function(id, period, params)}. Should return some HTML, or 
#'        \code{NEXT} if the participant can move directly to the next stage.
#' 
#' @details If \code{wait_for} is a function like 
#'        \code{function(id, period, params)}, then the participant will
#'        move on to \code{results} only when the function returns \code{TRUE}. If
#'        it is a list of vectors, then the participant will move on only when 
#'        all participants in the same vector are ready. For example, 
#'        \code{wait_for=list(1:4,5:8,9:12,13:16)} requires that participant
#'        IDs 1 to 4 are all ready before moving to \code{results}.
#' @examples
#' s1 <- structured_stage(
#'   form = function(id, period, params, error) {
#'     c("<html><body>", if(length(error)) c("<p style='color:red'>", error, 
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


#' @rdname stage
#' @export
NEXT <- -1

#' @rdname stage
#' @export
WAIT <- -2
