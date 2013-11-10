

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
      .handle_request <<- handler
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

TextStage <- setRefClass("TextStage", contains="AbstractStage",
  fields=list(
    html="character",
    shown="logical"
  ),
  methods=list(
    
  )
)

#' @export
NEXT <- -1

#' @export
WAIT <- -2

