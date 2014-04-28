#' @include servers.R
#' @include stages.R

Experiment <- setRefClass("Experiment",
  fields=list(
    stages="ANY",
    N="numeric",
    name="character",
    session_name="character",
    client_refresh="numeric",
    status=function(x) {
      if(missing(x)) return(.status)
      stopifnot(x %in% c("Stopped", "Waiting", "Started", "Paused"))
      .status <<- x
    },
    .status="character",
    .command_names="character",
    autostart="logical",
    allow_latecomers="logical",
    auth="ANY",
    server="Server",
    subjects="data.frame",
    requests="list",
    commands="list",
    start_time="POSIXct",
    clients_in_url="logical",
    env="environment"
  ),
  methods=list(
    initialize = function(..., auth=TRUE, port, autostart=FALSE, 
      allow_latecomers=FALSE, N=Inf, server="RookServer", name="betr", 
      client_refresh=10, clients_in_url=FALSE) {
      stages <<- list()
      env <<- new.env(parent=.GlobalEnv)
      subjects <<- data.frame(client=character(0), id=numeric(0), 
            period=numeric(0), status=factor(, levels=c("Running", "Waiting", 
            "Finished")), stringsAsFactors=FALSE)
      status <<- "Stopped"
      
      # server can be a class name, a class object (refObjectGenerator), 
      # or an actual Server object
      if (! inherits(server, "Server")) {
        server_args <- list(pass_request=.self$handle_request, 
              clients_in_url=clients_in_url, name=name)
        sclass <- if (is.character(server)) get(server) else server
        if (missing(port) && sclass$className %in% 
            c("RookServer", "CommandLineServer"))
          server_args$port <- 35538
        server <<- do.call(sclass$new, server_args)
      }
      requests <<- commands <<- list()
      .command_names <<- c("start", "pause", "restart", "next_period")
      callSuper(..., auth=auth, autostart=autostart, clients_in_url=clients_in_url,
            allow_latecomers=allow_latecomers, N=N, client_refresh=client_refresh,
            name=name)
      if (is.infinite(N)) warning("No maximum N set for experiment")
    },
    
    finalize = function(...) {
      if (inherits(server, "Server")) server$halt()
    },
    
    environment = function() env,
    
    add_stage = function(..., times, each, after) {
      if (status != "Stopped") 
        warning("Adding stage to server while status is ", status, 
              "... this is risky!")
      stgs <- list(...)
      for (i in 1:length(stgs)) {
        if (is.function(stgs[[i]])) stgs[[i]] <- Stage$new(handler=stgs[[i]])
      }
      if (! missing(times)) stgs <- rep(stgs, times=times)
      if (! missing(each)) stgs <- rep(stgs, each=each)
      if (missing(after)) after <- length(stages)
      stgs <- sapply(stgs, function (x) x$copy())
      sapply(stgs, function (x) x$set_environment(env))
      stages <<- append(stages, stgs, after=after)
    }, 
    
    waiting_page = function(message="") {
      sprintf("<html><head><meta http-equiv='refresh' content='%d'></head>
        <body>%s</body></html>", client_refresh, message)
    },
    
    special_page = function(message) {
      sprintf("<html><head></head><body>%s</body></html>", message)
    },
    
    authorize = function(client, params) {
      if (client %in% subjects$client) return(subjects[subjects$client==client,])
      # we have a new client:
      if (nrow(subjects) >= N) stop("Too many participants")
      if (! status %in% c("Started", "Waiting")) stop("Not accepting participants")
      if (status=="Started" && ! allow_latecomers) 
        stop("Experiment has already started")
      ok <- switch(class(auth),
        logical = auth,
        character = any(sapply(auth, grepl, x=client)),
        "function" = auth(client, params),
        stop("auth is of class '", class(auth), 
          "', should be character or function")
      )
      if (! ok) stop("Client unauthorized")
      id <- if(nrow(subjects)) max(subjects$id)+1 else 1
      subjects <<- rbind(subjects, data.frame(id=id, period=0, client=client, 
            status=factor("Running", levels=c("Running", "Waiting", "Finished")), 
            stringsAsFactors=FALSE))
      if (status=="Started") next_period(subjects[subjects$client==client,])
      # if we reach N, trigger a change of state
      if (nrow(subjects)==N && autostart) {
        start() # not via handle_command; will be autotriggered on replay
      }
      return(subjects[subjects$client==client,])
    },
    
    next_period = function(subj) {
      if (status != "Started") {
        warning("Experiment status is not 'Running', cannot move subjects on")
        return(invisible(FALSE))
      }
      if (is.numeric(subj)) subj <- subjects[subjects$id==subj,]
      done <- subjects$id %in% subj$id & subjects$period == length(stages)
      subjects$status[done] <<- "Finished"
      srows <- subjects$id %in% subj$id & subjects$period < length(stages)
      subjects$period[srows] <<- subjects$period[srows] + 1
      subjects$status[srows] <<- "Running" # do I need this?
      return(invisible(TRUE))
    },
    
    record_command = function(command, params) {
      if (missing(params)) params <- NULL
      command <- list(name=command, params=params, 
        time=Sys.time() - start_time)
      commands <<- append(commands, command)
      # TODO: print to file
      tmp <- file(file.path(session_name, "record", paste0("command-", 
            as.character(command$time))), open="w")
      cat(command$name, "\n", paste(mapply(paste, names(params), params, 
            MoreArgs=list(sep=":")), collapse="\n"), file=tmp)
      close(tmp)
    },
    
    record_request = function(client, params) {
      request <- list(client=client, params=params, 
        time=Sys.time() - start_time)
      requests <<- append(requests, request)
      tmp <- file(file.path(session_name, "record", 
            paste0("request-", as.character(request$time))), open="w")
      cat(client, "\n", paste(mapply(paste, names(params), params,
            MoreArgs=list(sep=":")), collapse="\n"), file=tmp)
      close(tmp)
    },
    
    handle_command = function(command, params) {      
      if (command %in% .command_names) {
        record_command(command, params) 
        command <- do.call(`$`, list(.self, command)) 
        if (missing(params)) command() else do.call(command, params)
      } else {
        warning("Got unrecognized command: ", command)
      }
    },
    
    handle_request = function(client, params) {
      record_request(client, params)
      # authorization
      subject <- tryCatch( authorize(client, params), error = function(e) e)
      if (inherits(subject, "error")) {
        return(special_page(conditionMessage(subject)))
      }
      return(.handle_request(subject, params, client))
    },
    
    .handle_request = function(subject, params, client) {
      switch(status, 
        Stopped = stop("Got handle_request but experiment is stopped"),
        Paused  = waiting_page("Experiment paused"),
        Waiting = waiting_page("Waiting to start"),
        Started = {
          if (subject$status=="Finished") return(special_page("Experiment finished"))
          stage <- stages[[subject$period]]
          client <- client
          # self_url <- function() ""
          # stage$.set_handler_env(environment())
          assign("period", subject$period, envir=env)
          assign("id", subject$id, envir=env)
          result <- stage$handle_request(subject$id, subject$period, params)
          if (is.next(result)) {
            next_period(subject)
            # NB we clean the params when the subject moves on. Is this OK?
            return(.handle_request(subjects[subjects$id==subject$id,], client=client))
          } else if (is.wait(result)) {
            subjects$status[subjects$id==subject$id] <<- "Waiting"
            return(waiting_page("Waiting for experiment to continue"))
          } else {
            return(result)
          }
        },
        stop("Unrecognized experiment status:", status)
      )
    },
    
    get_url = function() server$get_url(),
        
    info = function(subj=TRUE, map=TRUE) {
      cat(sprintf("%s\tStatus: %s\tClients: %d/%0.0f\tStages: %d\n", 
            ifelse(status=="Stopped", paste("Name:", name), paste("Session:",
            session_name)), status, nrow(subjects), N, length(stages)))
      if (status != "Stopped") server$info()
      if (subj && nrow(subjects) > 0) {
        cat("Subjects:\n")
        print(subjects)
      }
      if (map) .self$map()
    },
    
    get_session_name = function() {
      if (status=="Stopped") {
        warning("No session defined yet, status is Stopped")
        return(NA)
      } else {
        return(session_name)
      }
    },
    
    map = function() {
      tbl <- table(subjects$period)
      cat("Stage progression:\n")
      for (i in as.character(1:length(stages))) {
        if (i %in% names(tbl)) cat(i,":", rep("*", tbl[[i]]), "[", tbl[[i]], "]\n",
              sep="")
      }
    },

    ready = function() {
      if (status != "Stopped") {
        warning("Called ready() on an experiment with status ", status)
        return(invisible(FALSE))
      }
      start_time <<- Sys.time()
      session_name <<- paste(name, format(start_time, 
        "%Y-%m-%d-%H%M%S"), sep="-")
      dir.create(fp <- file.path(session_name, "record"), recursive=TRUE)
      if (file.access(fp, 2) != 0) stop("Could not write into ", fp)
      ## run server
      server$start(session_name=session_name)
      status <<- "Waiting"
      return(invisible(TRUE))
    },
    
    start = function(force=FALSE) {
      if (status != "Waiting") {
        warning("Experiment status is '", status, "' - call ready() first")
        return(invisible(FALSE))
      }
      if (nrow(subjects) < N && ! force) {
        warning("Number of subjects ", nrow(subjects), " < N = ", N, 
              " - not starting. Use force=TRUE to override")
        return(invisible(FALSE))
      }
      status <<- "Started"
      if (nrow(subjects) > 0) {
        next_period(subjects)
      } else {
        warning("Experiment started with no participants")
      }
      return(invisible(TRUE))
    },
    
    pause = function() {
      if (status != "Running") {
        warning("Experiment not running, cannot pause")
        return(invisible(FALSE))
      } else {
        status <<- "Paused"
        return(invisible(TRUE))
      }
    },
    
    restart = function() {
      if (status != "Paused") {
        warning("Experiment not paused, cannot restart")
        return(invisible(FALSE))
      } else {
        status <<- "Running"
        return(invisible(TRUE))
      }
    }
    
  )
)

setGeneric("print") # do I need this?

#' @rdname info
#' @aliases print,Experiment,ANY-method
#' @aliases show,Experiment,ANY-method
#' @usage experiment
setMethod("print", "Experiment", function(x, ...) x$info(FALSE, FALSE))
setMethod("show", "Experiment", function(object) object$info(FALSE, FALSE))

#' Create an experiment.
#' 
#' In betr, an experiment consists of one or more stages, as well
#' as global options defined when the experiment is created.
#' 
#' @param auth may be TRUE, FALSE, a character vector of regular expressions,
#'        or a function taking two arguments, client and params.
#' @param port what port to listen on
#' @param autostart logical. Start the experiment automatically when N 
#'        participants have joined?
#' @param allow_latecomers logical. Allow participants to join after the
#'        experiment has started?
#' @param N a numeric giving how many participants are required
#' @param server a class name (quoted or unquoted) of a betr::Server 
#'        subclass, or an instance object. Typical: "RookServer"
#' @param name the character name of the experiment, used in creating
#'        folders.
#' @param client_refresh numeric. How often should waiting clients refresh
#'        thier pages?
#' @param clients_in_url logical. If \code{TRUE}, client names can be specified 
#'        in the URL as e.g. experiment/client_name. Useful for testing, should
#'        be turned off in production!
#' @return an object of class Experiment.
#' 
#' @details 
#' An experiment is typically created in a source file, which also adds one or 
#' more stages to it using \code{\link{add_stage}}. When you run the experiment,
#' you source this file. Call \code{\link[=ready]{ready(experiment)}} when you
#' want subjects to be able to connect to the server. They will see a waiting
#' page which refreshes regularly. To see your experiment's status, call
#' \code{\link[=info]{info(experiment)}} or simply type \code{experiment} on the
#' command line. When you want the experiment to start, call 
#' \code{\link[=start]{start(experiment)}}.
#' 
#' An experiment has its own empty environment. Functions and brew files
#' passed into stages will be evaluated in this environment. When the experiment
#' is replayed, this environment is cleaned. It is a good idea to assign
#' variables into the experiment's environment rather than elsewhere:
#' \code{
#'   with(environment(expt), mydf <- data.frame(id=character(0), profit=character(0)))
#' }
#' This will keep your experiments replay-safe.
#' 
#' @examples
#' expt <- experiment(name='testing', port=12345, N=4)
#' add_stage(expt, function(...)"<html><body>Hello world!</body><html>")
#' ready(expt)
#' 
#' @export
experiment <- function (...) Experiment$new(...)

#' Add a stage to an experiment
#' 
#' @param experiment an Experiment object 
#' @param ... one or more Stage objects, or functions
#' @param times how many times to repeat the sequence of stages in \code{...}.
#'        All stages are repeated if this is a single number; if it is a
#'        vector it gives how many times to repeat each stage.
#' @param each how many times to repeat each individual stage
#' @param after Add stages after what period
#' @usage add_stage(experiment, ..., times, each, after)
#' @details
#' If functions are passed in to \code{add_stage}, Stage objects will 
#' automatically be created from them. 
#' Stage objects are \link[=ReferenceClasses]{reference classes}. However,
#' when added to the experiment, they are copied. So, changing the 
#' Stage after adding it to the experiment will not work.
#' @examples
#' expt <- experiment(N=1, autostart=TRUE)
#' s1 <- stage(function(id, period, params) return("Got to s1!"))
#' # Or just define the function directly:
#' s2 <- function(id, period, params) return("Got to s2!") 
#' add_stage(expt, s1, s2, times=2) # s1 s2 s1 s2
#' add_stage(expt, s1, s2, times=1:2) # s1 s2 s2
#' add_stage(expt, s1, s2, each=2) # s1 s1 s2 s2
#' info(expt)
#' @export
add_stage <- function (experiment, ...) 
      experiment$add_stage(...)

#' Start the experiment running.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{start} moves the experiment from Waiting to Started.
#' If the experiment has autostart set, this will happen automatically
#' when N subjects have connected. 
#' When the experiment starts, all subjects are moved to the first stage.
#' @param experiment Object of class Experiment
#' @param force Start the experiment even if there are fewer than N participants
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
start <- function(experiment, force=FALSE) experiment$handle_command("start",
      list(force=force))


#' Set the experiment up to receive participants.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{ready(experiment)} moves the experiment from Stopped to Waiting.
#' Clients can now connect and will be shown a waiting page.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
ready <- function(experiment) experiment$ready()


#' Pause the experiment.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{pause(experiment)} moves the experiment from Started to Paused.
#' Clients will be shown a waiting page until the experiment is continued.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
pause <- function(experiment) experiment$handle_command("pause")

#' Restart the experiment after pausing
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{restart(experiment)} moves the experiment from Paused back to Started.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
restart <- function(experiment) experiment$handle_command("restart")

#' Move some clients forward one period
#' 
#' Manually moves one or more clients forward. This may break your experimental
#' design so use in emergencies only!
#' 
#' @param experiment Object of class Experiment
#' @param subjid numeric vector of subject id(s) to move forward, or data 
#'        frame from subjects table
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
next_period <- function(experiment, subjid) {
  warning("Moving subject on manually, this may do bad things to your data")
  experiment$handle_command("next_period", list(subj=subjid))
}

#' Show basic info about an experiment
#' 
#' \code{info} prints information about the status, including session name,
#' number of stages, number of clients connected and total N, 
#' status (Stopped, Waiting, Started or Paused)
#' and the URL where the experiment is serving.
#' \code{map} shows a map of how subjects are progressing through the stages.
#' \code{get_url} returns the experiment url.
#' \code{session_name} returns the experiment session name, 
#' or NA if the experiment status is Stopped.
#' @param experiment an object of class Experiment
#' @param subj if TRUE, print the subjects table
#' @param map if TRUE, also calls \code{map}
#' 
#' @examples
#' expt <- experiment(N=2, port=12345)
#' expt # on the command line, calls info() 
#' 
#' @family command line functions
#' @export
info <- function(experiment, subj=TRUE, map=TRUE) experiment$info(subj, map)

#' @rdname info
#' @export
map <- function(experiment) experiment$map()

#' @rdname info
#' @export
get_url <- function(experiment) experiment$get_url()

#' @rdname info
#' @export
session_name <- function(experiment) experiment$get_session_name()

setGeneric("environment")
#' Return an experiment's environment
#' 
#' @examples
#' expt <- experiment(N=1)
#' with(environment(expt),
#'     mydf <- data.frame(id=numeric(0), profit=numeric(0))
#' )
#' 
#' @details
#' It is a good idea to assign variables within the experiment's environment
#' in your source file before defining stages. These variables can then be
#' accessed in your stage functions and brew files. They can be written to
#' using \code{<<-}. Doing this helps make your experiment replay-safe.
#' @export
setMethod("environment", "Experiment", function(fun) 
      fun$environment())
