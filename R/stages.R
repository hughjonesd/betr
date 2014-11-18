#' @import brew

#' @export AbstractStage
#' @exportClass AbstractStage
AbstractStage <- setRefClass("AbstractStage",
  fields=list(expt="ANY", name="character"),
  methods=list(
    initialize = function(name="Unnamed", ...) {
      name <<- name
      callSuper(...)
    },
    
    handle_request = function(id, period, params) stop(
          "handle_request called on object of class AbstractStage"),
    
    description = function() paste0(name, " (", class(.self) ,")")
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
#' @param name optional name of the stage.
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
stage <- function (handler, name="No name") Stage$new(handler=handler, name=name)


#' @export TextStage
#' @exportClass TextStage
TextStage <- setRefClass("TextStage", contains="AbstractStage",
  fields=list(
    page="ANY",
    shown="numeric",
    wait="logical"
  ),
  methods=list(
    initialize = function(page=NULL, wait=FALSE, ...) {
      page <<- page
      wait <<- wait
      callSuper(shown=numeric(0), ...)
    },
    
    handle_request = function(id, period, params) {
      if (id %in% shown && ! wait) return(NEXT)
      if (! id %in% shown) shown <<- c(shown, id)
      res <- call_page(page, id, period, params, errors=character(0))
      return(res)
    }
  )
)

#' Create a stage which does nothing but display HTML to the subject.
#' 
#' A text stage presents some HTML once to each subject.
#' 
#' @param name optional name of the stage.
#' @param page A character vector containing HTML, or a function to be called 
#'        with parameters \code{id, period, params}.
#' @param wait Wait to move on?
#' 
#' @return An object of class TextStage. When called the first time, this will
#'         display the HTML in \code{file} or \code{text} to the participant. 
#'         If \code{wait} is \code{FALSE} (the default), subsequent calls
#'         will return \code{NEXT}. If \code{wait} is \code{TRUE}, subsequent
#'         calls return the same page.
#' @details It is always safe to call \code{next_stage} on a participant who
#'          is at a TextStage.
#'          
#'          Functions such as \code{\link{b_brew}} can be used as arguments to
#'          \code{page}.
#'          
#' @family stages  
#' @export
text_stage <- function (page, wait=FALSE, name="No name") 
      TextStage$new(page=page, wait=wait, name=name)

rookify <- function (thing) {
  if (inherits(thing, "Response")) return(thing)
  rr <- Rook::Response$new()
  rr$write(thing)
  return(rr)
}



#' @export FormStage
#' @exportClass FormStage
FormStage <- setRefClass("FormStage", contains="AbstractStage",
  fields = list(
    fields       = "list",
    page    = "ANY",
    titles       = "ANY",
    data_frame   = "character",
    seenonce     = "numeric",
    multi_params = "character"
  ),
  methods = list(
    initialize = function(page=NULL, fields=list(), titles=NULL, 
          data_frame="", multi_params="AsIs", ...) {
      page <<- page
      if (! is.list(fields)) {
        fnames <- fields
        fields <- list()
        for(i in 1:length(fnames)) fields[[i]] <- has_value()
        names(fields) <- fnames
      }
      fields <<- fields
      titles <<- titles
      multi_params <<- multi_params
      if (! is.null(titles) && length(mf <- setdiff(names(fields), 
            names(titles)))) {
        stop("Missing fields from titles: ", paste(mf, sep=", "))
      }
      data_frame <<- data_frame
      seenonce <<- numeric(0)
      callSuper(...)
    },
    
    handle_request = function(id, period, params) {
      if (id %in% seenonce) {
        errs <- character(0)
        for (f in seq_along(fields)) {
          fname <- names(fields[f])
          f <- fields[[f]]
          ftitle <- if(is.null(titles)) fname else titles[[fname]]
          err <- f(ftitle, params[[fname]], id, period, params)
          if (! is.null(err)) {names(err) <- fname; errs <- c(errs, err)}
        }
        if (length(errs) == 0) {
          params <- lapply(params, type.convert, as.is=TRUE)
          # for inserting multiple values into an AsIs column
          f <- switch(multi_params, AsIs=list, paste=function(x) paste(x, 
                sep=","))
          params <- lapply(params, function(x) if (length(x)>1) f(x) else x )
          update_data_frame(id, period, params[names(fields)])
          return(NEXT)
        } else {
            res <- call_page(page, id, period, params, errors=errs)     
        }
      } else {
        seenonce <<- c(seenonce, id)
        error <- ""
        res <- call_page(page, id, period, params, errors=character(0))
      }
      return(res)
    },
    
    update_data_frame = function(id, period, params) {
      if (! is.data.frame(.GlobalEnv[[data_frame]])) stop("'", data_frame, 
            "' is not a data frame in the global environment")
      if (period < 1) warning("Period is not 1 yet, did you forget to include a period()?")
      selrow <- .GlobalEnv[[data_frame]]$id==id & 
            .GlobalEnv[[data_frame]]$period==period
      .GlobalEnv[[data_frame]][selrow, names(params)] <- params
    }
  )
)


#' Print out a form and store the subject's inputs in a data frame, after
#' checking for errors
#' 
#' @param page A character vector containing HTML, or a function to be called 
#'        with parameters \code{id, period, params, errors}.
#' @param fields A character vector of field names, or a list like 
#'        \code{list(field_name=check_function, ...)}
#' @param titles A list of field titles, like \code{list(field_name=title,...)}
#' @param data_frame The quoted string name of the data frame to be updated.
#'        This should exist in the global environment.
#' @param name optional name of the stage.
#' @param multi_params How to deal with multi-valued parameters. "AsIs"
#'        enters them as a list in a single column (which should be of type 
#'        AsIs); "paste" pastes them, separated by commas, in a single column.
#'         
#' 
#' @details 
#' 
#' When the subject first arrives, the text or file \code{page} is 
#' displayed. After the form is submitted, it is checked for errors, as follows:
#' each member of the list \code{fields} is called as a function, with arguments
#' \code{(field_title, value, id, period, params)}. Here
#' 
#' \itemize{
#'    \item \code{field_title} is the field name itself (the name of the 
#'          corresponding list element) unless a list of titles was supplied.
#'    \item \code{value} is the value of the corresponding field in the form
#'          submitted by the user. This will be a length 1 character string.
#'    \item \code{id} and \code{period} are self-explanatory
#'    \item \code{params} is the full list of parameters
#' } 
#' 
#' If the function returns \code{NULL} the field is OK. If the function
#' returns a character vector, this is treated as a vector of error messages.
#' 
#' If there are any error messages, \code{page} is redisplayed. If there
#' are no error messages, the data frame named in \code{data_frame} is updated: 
#' in the row with \code{id==id && period==period}, the columns
#' named by \code{fields} get the values passed in by the user. 
#' 
#' \code{\link{b_brew}} and similar functions can be used as arguments to 
#' \code{page}.
#' 
#' \code{\link{is_whole_number}} and similar functions return functions suitable
#' for use in the fields list.
#' 
#' Multiple parameters can be created by writing e.g.
#' 
#' \code{<input type='checkbox' name='fruit[]' value='banana'>
#' <input type='checkbox' name='fruit[]' value='peach'>
#' ...}
#' 
#' in the HTML page. Note that if \code{multi_params="paste"},
#' parameters are comma-separated without being quoted. This will cause problems
#' if you e.g. allow free text entry and subjects use commas. In these cases
#' it is better to use one of the other alternatives.
#' 
#' 
#' @examples
#' 
#' mydf <- data.frame(id=rep(1:5, each=5), period=rep(1:5, times=5), 
#'      username=NA_character_, password=NA_character_)
#' 
#' s1 <- form_stage(page=b_brew("myform.html"), data_frame="mydf",
#'  fields=list(
#'    username=length_between(8, 15),
#'    password=all_of(length_at_least(8), function(name, val, ...) {
#'      if (val=='password') return("Password should not be 'password'")
#'      if (grepl('&[A-Za-z0-9]$', val)) return("Password should contain at 
#'            least one non-numeric character")
#'      return(NULL)
#'      })
#' ))
#' 
#' 
#' @return An object of class FormStage
#' @family stages
#' @export
form_stage <- function (page, fields, titles=NULL, data_frame, name="No name",
      multi_params="AsIs") {
  if (missing(page) || is.null(page)) stop("page must be specified")
  if (missing(data_frame) || ! is.character(data_frame)) 
        stop("data_frame must be a string")
  if (missing(fields)) stop("Please specify a list of fields")
  FormStage$new(page=page, fields=fields, titles=titles, 
        data_frame=data_frame, name=name, multi_params=multi_params)
}


CheckPoint <- setRefClass("CheckPoint", contains="AbstractStage",
  fields = list(
    wait_for = "ANY",
    ready = "numeric"
  ),
  methods = list(
    initialize = function(wait_for="all", ...) {
      callSuper(wait_for=wait_for, ready=numeric(0), ...) 
    },
    
    check_ready = function(id) {
      if (length(wait_for)==1 && wait_for=="ever") return(FALSE)
      ids <- if (length(wait_for)==1 && wait_for=="all") 1:expt$N else 
        if (length(wait_for)==1 && wait_for=="none") numeric(0) else 
          which(wait_for==wait_for[id])
      return(all(ids %in% ready))
    },
    
    handle_request = function(id, period, params) {
      if (! id %in% ready) ready <<- c(ready, id)
      if (check_ready(id)) return(NEXT) else return(WAIT)
    }
  )
)


#' Make subjects wait for other subjects.
#' 
#' @param wait_for "all", "none", "ever" or a vector of length N
#' @param name optional name of the stage.
#' 
#' @details 
#' 
#' If \code{wait_for} is "all", then subjects must wait till all subjects in the
#' experiment have arrived. If \code{wait_for} is "none" then checkpoint does
#' nothing. If \code{wait_for} is "ever" then subjects wait forever (or until
#' moved on manually by the experimenter, using \code{\link{next_stage}}).
#' 
#' If \code{wait_for} is a vector, then it is assumed to represent
#' subject groups. So, if \code{wait_for} is \code{vec} then a subject with 
#' ID \code{x} will wait here until all subjects with IDs 
#' \code{which(vec==vec[x])} have arrived at this stage. 
#' 
#' A typical use of this might be \code{mydf$groups[order(mydf$id),]}
#' 
#' If all relevant subjects are ready, the subject moves on.
#' 
#' @examples
#' expt <- experiment(N=4)
#' groups <- c("A", "A", "B", "B")
#' s1 <- text_stage(page="<html><body><form action=''>
#'      <input type='submit' value='Next'></form></body></html>")
#'      
#' # wait for everyone:
#' add_stage(expt, period(), s1, checkpoint(), s1) 
#' 
#' # players 1 and 2 wait for each other, so do 3 and 4:
#' add_stage(expt, period(), s1, checkpoint(c(1, 1, 2, 2)), s1) 
#' 
#' @return An object of class CheckPoint
#' @family stages
#' @export
checkpoint <- function (wait_for="all", name="No name") 
      CheckPoint$new(wait_for=wait_for, name=name)

Period <- setRefClass("Period", contains="CheckPoint",
  fields = list(
  ),
  methods = list(
    handle_request = function(id, period, params) {
      if (! id %in% ready) ready <<- c(ready, id)
      if (check_ready(id)) {
        expt$next_period(id)
        return(NEXT)
      } else return(WAIT)
    }
  )
)

#' Begin a new period, optionally waiting for other subjects
#' 
#' This creates a period object which can be passed to \code{\link{add_stage}}.
#' Periods do nothing but add one to the period counter, and optionally
#' wait for all subjects to reach the same position.
#' 
#' @usage period(wait_for="none")
#' @param wait_for 
#' @param name Optional name of the stage 
#' 
#' @details 
#' 
#' A period may contain one or more stages. When the experiment is started,
#' all subjects are in period 0, which can be used for e.g. instructions.
#' Typically, you will want to put a new period before each
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
#' I\code{wait_for} is interpreted just as in \code{\link{checkpoint}}.
#' 
#' If all relevant subjects are ready, the subject's period counter is
#' incremented and the subject moves on.
#' 
#' @examples
#' expt <- experiment(N=4)
#' groups <- c("A", "A", "B", "B")
#' s1 <- text_stage(page="<html><body><form action=''>
#'      <input type='submit' value='Next'></form></body></html>")
#'      
#' # go ahead individually:
#' add_stage(expt, period(), s1) 
#' 
#' # wait for everyone:
#' add_stage(expt, period("all"), s1) 
#' 
#' # players 1 and 2 wait for each other, so do 3 and 4:
#' add_stage(expt, period(groups), s1) 
#' 
#' @return An object of class Period
#' @family stages
#' @export
period <- function (wait_for="none", name="No name") Period$new(
      wait_for=wait_for, name=name)




Program <- setRefClass("Program", contains="AbstractStage",
  fields = list(
    fn = "ANY",
    ready = "numeric",
    run = "ANY"
  ),
  methods = list(
    initialize = function(run=NULL, fn=NULL, ...) {
      callSuper(fn=fn, run=run, ready=numeric(0), ...) 
    },
    
    handle_request = function(id, period, params) {
      if (! id %in% ready) ready <<- c(ready, id)
      if ((length(ready) == 1 && run == "first") || run == "all" || 
            (length(ready) == expt$N && run == "last")) fn(id, period)    
      return(NEXT)
    }
  )
)


#' Run a function, either once or for each subject
#' 
#' @param run "first", "last", or "all".
#' @param fn a function which should take two arguments, \code{id} and 
#' \code{period}.
#' @param name optional name of the stage.
#' 
#' @details 
#' If
#' \code{run} is \code{"all"} then the function \code{fn} will be run every time 
#' a subject reaches the stage. If \code{run} is \code{"first"} then \code{fn}
#' will be run when the first subject reaches the stage. If \code{run} is 
#' \code{"last"} then \code{fn} will be run when the last subject reaches
#' the stage.
#' 
#' @examples
#' 
#' expt <- experiment(N=16, on_ready=function() {
#'  mpcr <<- 1.5
#'  mydf <<- experiment_data_frame(expt)
#'  mydf$contrib <<- NA
#'  mydf$profit <<- NA
#'  mydf$group <<- rep(rep(1:4, each=4), nperiods(expt))
#' })
#' 
#' s1 <- function(id, period, params) {
#'  if (! is.null(params) && 'contrib' %in% names(params)) {
#'    mydf$contrib[mydf$id==id & mydf$period==period] <<- params$contrib
#'    return(NEXT)
#'  } else {
#'    return("<html><body><form action=''>
#'      Enter a contribution:<input name='contrib' type='text'>
#'      <input type='submit' value='Next'></form></body></html>")
#'  }
#' }
#' 
#' s2 <- program("last", function(id, period) {
#'  mydf$profit <<- with(mydf[mydf$period==period,],
#'        ave(contrib, group, FUN=function(x) 50 - x + mpcr * mean(x)))
#' })
#' 
#' add_stage(expt, period(), s1, checkpoint(mydf$group), s2, s3, times=10)
#'      
#' @return A Stage object of class Program
#' @family stages
#' @export
#                                       list() to catch errors
program <- function (run, fn, name="No name") {
  list(run,fn);
  Program$new(run=run, fn=fn, name=name)
} 

Timed <- setRefClass("Timed", contains="AbstractStage",
  fields=list(
    timeout="numeric",
    on_timeout="function",
    stage="AbstractStage",
    started="numeric",
    expt=function(x) {
      if (missing(x)) return(stage$expt)
      stage$expt <- x
    }
  ),
  methods=list(
    initialize = function(stage=NULL, timeout=0, on_timeout=function(...) NULL, ...) {
      if (! is.null(stage)) stage <<- stage
      timeout <<- timeout
      on_timeout <<- on_timeout
      started <<- numeric(0)
      callSuper(...)
    },
    
    handle_request = function(id, period, params) {
      if (is.na(started[id])) {
        started[id] <<- expt$elapsed_time()
        time_remaining <- timeout
      } else {
        time_passed <- expt$elapsed_time() - started[id]
        if (time_passed > timeout) {
          on_timeout(id, period)
          return(NEXT)
        } else {
          time_remaining <- timeout - time_passed 
        }
      }
      res <- stage$handle_request(id, period, params)
      if (is.next(res) || is.wait(res)) return(res) # shd WAIT interrupt t.o.?
      if (! is(res, "Response")) res <- rookify(res)
      res$header("Refresh", time_remaining)  
      return(res)
    },
    
    description = function() paste0(stage$name, " (Timed ", class(stage), ")")
  )
)

#' Add a timeout to a stage
#' 
#' @param stage A Stage object, or a function
#' @param timeout timeout length in seconds
#' @param on_timeout a function to be called if a subject times out.
#' 
#' @details 
#' 
#' Returns a Stage which contains the original \code{stage}.
#' If a subject times out, `NEXT` is returned and `on_timeout` is called with
#' arguments `(id, period)`.
#' 
#' Timed stages are implemented using 
#' \href{http://en.wikipedia.org/wiki/Meta_refresh}{http refresh}. This is not a
#' standard part of the HTTP specification, so use with caution if you do not 
#' control which browsers your subjects will be running.
#' 
#' @examples
#' s1 <- form_stage(c(header(), "Enter something", 
#'      "<form action='' method='POST'><input name='foo'></form>", footer()), 
#'      fields=list(foo=has_value()), data_frame="mydf")
#' # set a default value:
#' s1_timed <- timed(s1, 60, on_timeout=function(id, period) 
#'      mydf$foo[mydf$id==id & mydf$period==period] <<- "Default value"))
#' 
#' @return A Stage object of class Timed
#' @family stages
#' @export
timed <- function (stage, timeout, on_timeout=function(...) NULL) {
  if (is.function(stage)) {
    stage <- Stage$new(handler=stage)
  } 
  if (! inherits(stage, "AbstractStage")) stop("stage must be a Stage object")
  if (! is.numeric(timeout) || timeout <= 0) 
        stop("timeout must be a positive number, was ", timeout)
  Timed$new(stage=stage, timeout=timeout, on_timeout=on_timeout)
}

description <- function(stage) stage$description()

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

