

AbstractStage <- setRefClass("AbstractStage",
  fields=list(
    timeout="numeric"
  ),
  methods=list(
    initialize=function(...) callSuper(...),
    
    handle_request=function(id, period, params) stop(
          "handle_request called on object of class AbstractStage")
  )
)

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
#'     containing HTML, or the constant WAIT, or the constant NEXT.
#' @return a Stage object suitable for adding to an experiment.
#' @examples
#' stg <- stage(function(id, period, params) {
#'  if(params$done=="OK") return(NEXT)
#'  paste0("<html><body><p>Your ID is ", id, " and the period is", period,
#'        "</p><form action='", self_url, "' method=POST>
#'        <input type='Submit' name='done' value='OK'></form>")
#' })
#' @export
stage <- function (handler) Stage$new(handler)

.file_or_brew <- function(fb) {
  # WTF does brew() not just return a string?
  if (grepl("\\.brew$", fb)) capture.output(brew(fb)) else readLines(fb)
}

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

#' Create a stage which simply shows some HTML to the user
#' 
#' A text stage presents some HTML once to each subject
#' 
#' @param file A filepath. If the file ends in ".brew" then it is passed to 
#'        \code{brew} for processing. Otherwise it is shown to the subject as-is.
#' @param text A character vector of HTML. Only one of \code{file} and \code{text}
#'        should be passed.
#' @return an object of class TextStage.
#' @export
text_stage <- function (...) TextStage$new(...)

#' @export
NEXT <- -1

#' @export
WAIT <- -2

