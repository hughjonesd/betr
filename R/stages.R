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
    page="ANY",
    shown="numeric",
    wait="logical"
  ),
  methods=list(
    initialize = function(page, wait=FALSE, ...) {
      page <<- page
      wait <<- wait
      callSuper(shown=numeric(0), ...)
    },
    
    handle_request = function(id, period, params) {
      if (id %in% shown && ! wait) return(NEXT)
      if (! id %in% shown) shown <<- c(shown, id)
      res <- call_page(page, id, period, params)
      return(res)
    }
  )
)

#' Create a stage which does nothing but display HTML to the subject.
#' 
#' A text stage presents some HTML once to each subject.
#' 
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
#'  \code{function(id, period, params, error)}. \code{params} and \code{error}
#'  may both be missing.
#'  
#'  If \code{result} is a function, it should take three arguments, like 
#'  \code{function(id, period, params)}. \code{params} may be missing.
#'  
#' If \code{wait_for} is a function, it should be of the form 
#'        \code{function(id, period, params, ready)}. \code{ready} is a vector
#'        containing the ids of participants who have been marked as ready. The
#'        participant will move on to \code{results} only when the function
#'        returns \code{TRUE}. 
#'        
#'  If \code{wait_for} is a list of vectors, then the participant will move on
#'  only when all participants in the same vector are ready. For example,
#'  \code{wait_for=list(1:4,5:8,9:12,13:16)} requires that participant IDs 1 to
#'  4 are all ready before participant 1 can move to \code{results}.
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


#' Functions which return functions that can check a form input for errors.
#' 
#' These functions return functions which can be passed to the \code{fields}
#' list of a \code{\link[form_stage]{FormStage}} object. The created functions 
#' check their inputs and return \code{NULL} or an error message.
#' 
#' \code{all_of} checks each of the functions it is passed.
#' 
#' @examples
#' f1 <- is_whole_number()
#' f1("pi", 3.141)
#'
#' 
#' 
#' @return A function for checking a form input
#' @family checks
#' @export
all_of <- function(...) {
  subchecks <- list(...)
  function(ftitle, val, ...) {
    results <- lapply(subchecks, function(sc) sc(ftitle, val, ...))
    nulls <- sapply(results, is.null)
    if (all(nulls)) return(NULL) 
    results <- results[!nulls]
    if (length(results)==1) return(results[[1]])
    return(c(paste0(ftitle, " has multiple errors:"), unlist(results)))
  }
}

#' @rdname all_of
#' @export
is_at_least <- function(min) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (! val >= min) paste0(ftitle, 
      " must be at least ", min) else NULL
  }
}

#' @rdname all_of
#' @export
has_value <- function() {
  function(ftitle, val, ...) {
    if (is.null(val) || is.na(val) || nchar(val)==0 ) paste0("Please submit a
          value for ", ftitle) else NULL
  }
}

#' @rdname all_of
#' @export
is_at_most <- function(max) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (is.null(val) || is.na(val) || ! val <= max) paste0(ftitle, 
      " must be no more than ", max) else NULL
  }
}

#' @rdname all_of
#' @export
is_whole_number <- function() {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    tol = .Machine$double.eps^0.5  
    if (is.null(val) || is.na(val) || abs(val - round(val)) >= tol) paste0(ftitle, 
          " must be a whole number") else NULL
  }
}

#' @rdname all_of
#' @export
is_between <- function(min, max) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (is.null(val) || is.na(val) || ! (val >= min && val <= max)) paste0(ftitle, 
        " must be between ", min, " and ", max) else NULL
  }
}

#' @rdname all_of
#' @export
length_between <- function(min, max) {
  function(ftitle, val, ...) {
    if (min > 0 && ! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    nc <- nchar(val)
    if (! (nc >= min && nc <= max)) paste0(ftitle, 
      " must be between ", min, " and ", max, " characters long") else NULL
  }
}


#' @rdname all_of
#' @export
length_at_least <- function(min) {
  function(ftitle, val, ...) {
    nc <- nchar(val)
    if (min > 0 && ! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    if (! nc >= min) paste0(ftitle, " must be at least ", min, 
          " characters long") else NULL
  }
}

#' @export FormStage
#' @exportClass FormStage
FormStage <- setRefClass("FormStage", contains="AbstractStage",
  fields = list(
    fields       = "list",
    page    = "ANY",
    titles       = "ANY",
    data_frame   = "character",
    seenonce     = "numeric"
  ),
  methods = list(
    initialize = function(page=NULL, fields=list(), titles=NULL, 
          data_frame="", ...) {
      page <<- page
      if (! is.list(fields)) {
        fnames <- fields
        fields <- list()
        for(i in 1:length(fnames)) fields[[i]] <- has_value()
        names(fields) <- fnames
      }
      fields <<- fields
      titles <<- titles
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
        for (f in 1:length(fields)) {
          fname <- names(fields[f])
          f <- fields[[f]]
          ftitle <- if(is.null(titles)) fname else titles[[fname]]
          err <- f(ftitle, params[[fname]], id, period, params)
          if (! is.null(err)) errs <- append(errs, err)
        }
        if (length(errs) == 0) {
          update_data_frame(id, period, params[names(fields)])
          return(NEXT)
        } else {
            error <- paste(errs, collapse="<br />") 
            res <- call_page(page, id, period, params)     
        }
      } else {
        seenonce <<- c(seenonce, id)
        error <- ""
        res <- call_page(page, id, period, params)
      }
      res <- gsub("<%\\s*errors\\s*%>", error, res) # for case of not brew      
      return(res)
    },
    
    update_data_frame = function(id, period, params) {
      if (! is.data.frame(.GlobalEnv[[data_frame]])) stop("'", data_frame, 
            "' is not a data frame in the global environment")
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
#'        with parameters \code{id, period, params}.
#' @param fields A character vector of field names, or a list like 
#'        \code{list(field_name=check_function, ...)}
#' @param titles A list of field titles, like \code{list(field_name=title,...)}
#' @param data_frame The quoted string name of the data frame to be updated.
#'        This should exist in the global environment.
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
#' If there are any error messages, \code{page} is redisplayed.
#' 
#'   If there
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
form_stage <- function (page, fields, titles=NULL, data_frame) {
  if (missing(page) || is.null(page)) stop("page must be specified")
  if (missing(data_frame) || ! is.character(data_frame)) 
        stop("data_frame must be a string")
  if (missing(fields)) stop("Please specify a list of fields")
  FormStage$new(page=page, fields=fields, titles=titles, 
        data_frame=data_frame)
}


CheckPoint <- setRefClass("CheckPoint", contains="AbstractStage",
  fields = list(
    wait_for = "ANY",
    ready = "numeric"
  ),
  methods = list(
    initialize = function(wait_for="all") {
      callSuper(wait_for=wait_for, ready=numeric(0)) 
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
#' s1 <- text_stage(text="<html><body><form action=''>
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
checkpoint <- function (...) CheckPoint$new(...)

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
#' @usage period(wait_for="none")
#' @param wait_for 
#' 
#' @details 
#' 
#' I\code{wait_for} is interpreted just as in \code{\link{checkpoint}}.
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
period <- function (wait_for="none") Period$new(wait_for=wait_for)




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
      if (length(ready) == 1 && run == "first" || run == "all" || 
            length(ready) == expt$N && run == "last") fn(id, period)    
      return(NEXT)
    }
  )
)


#' Run a function, either once or for each subject
#' 
#' @param run "first", "last", or "all".
#' @param fn a function which should take two arguments, \code{id} and 
#' \code{period}.
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
#'  mydf$profit <<- with(mydf[mydf$period==period,] 
#'        ave(contrib, group, FUN=function(x) 50 - x + mpcr * mean(x)))
#' })
#' 
#' add_stage(expt, period(), s1, checkpoint(mydf$group), s2, s3, times=10)
#'      
#' @return A Stage object of class Program
#' @family stages
#' @export
program <- function (run, fn) {list(run,fn);Program$new(run, fn)} # list() to catch errors

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
    }
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

