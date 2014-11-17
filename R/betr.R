#' @import yaml

#' @include servers.R
#' @include stages.R

#' @export Experiment
#' @exportClass Experiment
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
    randomize_ids="logical",
    random_ids="numeric",
    auth="ANY",
    server="Server",
    .oldserver="Server",
    subjects="data.frame",
    seats="data.frame",
    on_ready="ANY",
    requests="list",
    commands="list",
    clients_in_url="logical",
    record="logical",
    seed="integer"
  ),
  methods=list(
    initialize = function(..., auth=TRUE, port, autostart=FALSE, 
      allow_latecomers=FALSE, N=Inf, server="RookServer", name="betr", 
      client_refresh=5, clients_in_url=FALSE, seats_file="betr-SEATS.txt",
      on_ready=NULL, randomize_ids=TRUE, record=TRUE, seed=NULL) {
      stages <<- list()
      initialize_subjects()
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
      .command_names <<- c("start", "pause", "restart", "next_stage")
      seats <<- data.frame(seat=numeric(0), IP=character(0), cookie=character(0))
      if (! is.null(seats_file) && nzchar(seats_file)) {
        err <- try(seats <<- read.table(seats_file, header=TRUE, 
              colClasses=c("integer", "character", "character")), silent=TRUE)    
        if (class(err)=="try-error") warning("Problem reading seats file ", seats_file)
      }
      if (! is.null(seed)) seed <<- as.integer(seed) else {
        seed <<- sample(1:10000000, 1) # hopefully random
      }
      callSuper(..., auth=auth, autostart=autostart, clients_in_url=clients_in_url,
            allow_latecomers=allow_latecomers, N=N, client_refresh=client_refresh,
            name=name, on_ready=on_ready, randomize_ids=randomize_ids, 
            record=record)
      if (is.infinite(N)) warning("No maximum N set for experiment")
    },
    
    initialize_subjects = function() {
      subjects <<- data.frame(client=character(0), IP=character(0), 
            id=numeric(0), seat=character(0), period=numeric(0), 
            status=factor(, levels=c("Running", "Waiting", "Finished")), 
            stage=numeric(0), stringsAsFactors=FALSE)
    },
    
    finalize = function(...) {
      if (status != "Stopped") halt(force=TRUE)
    },
        
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
      sapply(stgs, function (x) x$expt <- .self)
      stages <<- append(stages, stgs, after=after)
    }, 
        
    nperiods = function() {
      if (length(stages)==0) return(0) # avoid zero-length error
      length(stages[sapply(stages, inherits, "Period")])
    },
    
    elapsed_time = function() {
      if (status=="Stopped") stop("Called get_time but experiment is Stopped")
      # if we are replaying need to fake time!
      server$elapsed_time()
    },
    
    waiting_page = function(message="") {
      paste0(header(refresh=client_refresh), message, footer())
    },
    
    special_page = function(message) {
      paste0(header(refresh=client_refresh), message, footer())
    },
    
    authorize = function(client, params, ip, cookies) {
      if (is.null(ip)) ip <- NA
      if (client %in% subjects$client) return(subjects[subjects$client==client,])
      
      # we have a new client:
      if (nrow(subjects) >= N) stop("Too many participants")
      if (! status %in% c("Started", "Waiting")) stop("Not accepting participants")
      if (status=="Started" && ! allow_latecomers) 
        stop("Experiment has already started")
      ok <- switch(class(auth),
        logical = auth,
        character = any(sapply(glob2rx(auth), grepl, x=ip)),
        "function" = auth(ip, params, cookies),
        stop("auth is of class '", class(auth), 
          "', should be TRUE, FALSE, character or function")
      )
      if (! ok) stop("Client unauthorized")

      id <- if(nrow(subjects)) length(unique(subjects$id))+1 else 1
      if (randomize_ids) id <- random_ids[id]
      seat <- NA
      if (nrow(seats)>0) {
        if (! is.null(cookies) && "betr-seat" %in% cookies && nrow(seats)>0) {
          seat <- seats$seat[ seats$cookie==cookies[["betr-seat"]] ] 
        } else if (! is.na(ip)) {
          seat <- seats$seat[seats$IP==ip] 
        } 
        if (is.na(seat)) warning("Seat not found for client with IP address '", ip, 
                if (! is.null(cookies)) paste0(", cookie '", 
                cookies[["betr-seat"]], "'"))
      }
      subjects <<- rbind(subjects, data.frame(client=client, IP=ip, id=id, 
            seat=seat, period=0, status=factor("Running", 
            levels=c("Running", "Waiting", "Finished")), stage=0, 
            stringsAsFactors=FALSE))
          
      if (status=="Started") next_period(subjects[subjects$client==client,])
      # if we reach N, trigger a change of state
      if (nrow(subjects)==N && autostart) {
        start() # not via handle_command; will be autotriggered on replay
      }
      return(subjects[subjects$client==client,])
    },
    
    next_stage = function(subj) {
      if (is.numeric(subj)) subj <- subjects[subjects$id %in% subj,]
      done <- subjects$id %in% subj$id & subjects$stage == length(stages)
      subjects$status[done] <<- "Finished"
      srows <- subjects$id %in% subj$id & subjects$stage < length(stages)
      subjects$stage[srows] <<- subjects$stage[srows] + 1
      subjects$status[srows] <<- "Running" # do I need this?
    },
    
    next_period = function(subjid) {
      if (status != "Started") {
        warning("Experiment status is not 'Running', cannot move subjects on")
        return(invisible(FALSE))
      }
      srows <- subjects$id %in% subjid
      subjects$period[srows] <<- subjects$period[srows] + 1
      return(invisible(TRUE))
    },
    
    record_command = function(command, params) {
      if (missing(params)) params <- NULL
      command <- list(name=command, params=params)
      commands <<- append(commands, command)
      tm <- elapsed_time()
      cat(as.yaml(command), file=file.path(session_name, "record", paste0("command-", 
            as.character(tm), "-", length(commands))))
    },
    
    record_request = function(client, params, ip=NULL, cookies=NULL) {
      request <- list(client=client, params=params, ip=ip, cookies=cookies)
      requests <<- append(requests, request)
      tm <- elapsed_time()
      cat(as.yaml(request), file=file.path(session_name, "record", 
            paste0("request-", as.character(tm), "-", length(requests))))
    },
    
    handle_command = function(command, params) {      
      if (command %in% .command_names) {
        if (record) record_command(command, params)
        command <- do.call(`$`, list(.self, command)) 
        if (missing(params)) command() else do.call(command, params)
      } else {
        warning("Got unrecognized command: ", command)
      }
    },
    
    handle_request = function(client, params, ip=NULL, cookies=NULL) {
      if (record) record_request(client, params, ip, cookies)
      # authorization
      subject <- tryCatch( authorize(client, params, ip, cookies), error = function(e) e)
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
          stage <- stages[[subject$stage]]
          client <- client
          result <- stage$handle_request(subject$id, subject$period, params)
          if (is.next(result)) {
            next_stage(subject)
            # NB we clean the params when the subject moves on. Is this OK?
            return(.handle_request(subjects[subjects$id==subject$id,], 
                  client=client, params=list()))
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
      cat(sprintf("%s\tStatus: %s\tClients: %d/%0.0f\tPeriods: %d\tStages: %d\n", 
            ifelse(status=="Stopped", paste("Name:", name), paste("Session:",
            session_name)), status, nrow(subjects), N, nperiods(), 
            length(stages)))
      if (status != "Stopped") server$info()
      if (subj && nrow(subjects) > 0) {
        cat("Subjects:\n")
        print(cbind(subjects[order(subjects$id),], 
              desc=sapply(subjects$stage, function(x) if (x==0) "" else 
              description(stages[[x]]))))
      }
      if (map) .self$map()
      invisible(TRUE)
    },
    
    get_session_name = function() {
      if (length(session_name)==0) {
        warning("No session name defined yet, maybe ready() has not been called")
        return(NA)
      }
      return(session_name)
    },
    
    map = function() {
      if (nrow(subjects) < 1) return()
      tbl <- table(subjects$period)
      cat("Period progression:\n")
      for (i in as.character(0:max(subjects$period))) {
        if (i %in% names(tbl)) cat(i,": ", rep(".", tbl[[i]]), " [", tbl[[i]], "]\n",
              sep="")
      }
    },
    
    print_stages = function() {
      for (i in 1:length(stages)) {
        cat(i, ": ", description(stages[[i]]), "\n", sep="")
      }
    },
    
    merge_subjects = function(data_frame) {
      merge(data_frame, subjects[,c("id", "IP", "client", "seat")], by="id", all.x=TRUE)
    },

    ready = function() {
      if (status != "Stopped") {
        warning("Called ready() on an experiment with status ", status, 
              "; ignoring since status is not Stopped.")
        return(invisible(FALSE))
      }
      session_name <<- paste(name, format(Sys.time(), 
        "%Y-%m-%d-%H%M%S"), sep="-")
      set.seed(seed)
      if (record) {
        dir.create(fp <- file.path(session_name, "record"), recursive=TRUE)
        if (file.access(fp, 2) != 0) stop("Could not write into ", fp)
        cat(seed, file=file.path(session_name, "seed"))
      }
      if (randomize_ids) random_ids <<- sample(1:N)
      if (! is.null(on_ready)) on_ready()
      status <<- "Waiting"
      server$start(session_name=session_name)
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
        next_stage(subjects)
      } else {
        warning("Experiment started with no participants")
      }
      return(invisible(TRUE))
    },
    
    pause = function() {
      if (status != "Started") {
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
        status <<- "Started"
        return(invisible(TRUE))
      }
    },
    
    halt = function(force=FALSE) {
      if (status == "Stopped") {
        warning("Experiment status is Stopped already, cannot halt")
        return(invisible(FALSE))
      }
      if (! force && any(subjects$status != "Finished")) {
        warning("Not all subjects have status Finished, not halting")
        return(invisible(FALSE))
      }
      server$halt()
      status <<- "Stopped"      
      return(invisible(TRUE))
    },
    
    replay = function(folder=NULL, maxtime=NULL, speed=NULL, ask=FALSE, 
          live=FALSE, clients=NULL) {
      if (live && (missing(speed) || is.null(speed))) speed="realtime"
      # if folder is null, use session_name or guess the most recently modified
      if (is.null(folder)) {
        if (length(session_name) == 0) {
          dirs <- file.info(list.files(pattern=paste0("^",name, 
            "-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{6}$"), include.dirs=TRUE))
          dirs <- dirs[order(dirs$mtime, decreasing=TRUE),]
          dirs <- dirs[dirs$isdir,]
          folder <- rownames(dirs[1,])
        } else folder <- session_name 
      }
      # if we've started, change status and halt the server
      if (status != "Stopped" && ! live) {
        status <<- "Stopped"      
        server$halt()        
      }
      .oldserver <<- server
      # clean up the subjects table etc.
      requests <<- commands <<- list()
      initialize_subjects()
      fp <- file.path(folder, "seed")
      err <- try(seed <<- as.integer(readLines(fp, warn=FALSE)), silent=TRUE)
      if (inherits(err, "try-error")) stop("Couldn't read seed file in ", fp)
      # start a replayserver which runs the commands
      server <<- ReplayServer$new(folder=folder, 
        pass_request=.self$handle_request, pass_command=.self$handle_command,
        name=name, speed=speed, maxtime=maxtime, ask=ask, clients=clients)
      ready() # this will create a new session (good idea?)
      # if we have autostart, then ready() will get the server running and do everything
      # if we don't have autostart, then a "start" command will be read in
      # so now a bunch of stuff gets called... 
      # ...
      # but do we ever get here?
      server$halt()
      server <<- .oldserver
      if (! live) server$start()
      # hopefully we are now back at the approp. period and with correct status...
      # need to think, how could we mix manual and automated clients...      
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
#' @param auth TRUE, FALSE, a character vector of patterns,
#'        or a function. See Details
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
#'        their pages?
#' @param clients_in_url logical. If \code{TRUE}, client names can be specified 
#'        in the URL as e.g. experiment/client_name. Useful for testing, should
#'        be turned off in production!
#' @param seats_file path of the file where seat information is stored. See
#'        \code{\link{identify_seats}} for details. Note: to suppress warnings
#'        about a missing file, use \code{seats_file=NULL}.
#' @param on_ready a user-defined function, to be called when \code{\link{ready}} 
#'        is called.
#' @param randomize_ids if \code{TRUE}, subject IDs will be randomized from
#'        1 to \code{N}. If \code{FALSE} subject IDs will be allocated first-come
#'        first-served.
#' @param record records experiment commands to disk. Turning this off will save
#'        disk space and not clutter your working directory, but will prevent
#'        experiment replay.
#' @param seed a seed to set whenever \code{\link{ready}} is called. You should
#'        ensure you set this to a different value in every session. 
#'        
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
#' To keep your experiments replay-safe, use \code{\link{on_ready}} to 
#' initialize your data.
#' 
#' The parameter \code{auth} determines how you authorize clients. \code{TRUE} 
#' (the default) allows any client to join the experiment. If \code{auth} is a
#' character vector, it is treated as a list of patterns in shell-glob style, e.g.
#' \code{"192.168.*.*"} (see \code{\link{glob2rx}} for details). If the
#' client's IP address matches any pattern, the client will be
#' accepted. IF \code{auth} is a function, it will be called like \code{auth(ip,
#' params, cookies)} where \code{ip} is the remote IP address and \code{params}
#' and \code{cookies} are lists of HTTP parameters and cookies respectively. The
#' client will be authorized if the function returns \code{TRUE}.
#' 
#' @examples
#' expt <- experiment(name='testing', port=12345, N=4)
#' add_stage(expt, function(...)"<html><body>Hello world!</body><html>")
#' ready(expt)
#' 
#' @export
experiment <- function (...) Experiment$new(...)

#' Add one or more stages to an experiment
#' 
#' @param experiment an Experiment object 
#' @param ... one or more Stage objects, or functions
#' @param times how many times to repeat the sequence of stages in \code{...}.
#'        All stages are repeated if this is a single number; if it is a
#'        vector it gives how many times to repeat each stage.
#' @param each how many times to repeat each individual stage
#' @param after Add stages after how many stages (default: at the end)
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

setGeneric("start") 
#' Start the experiment running.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{start} moves the experiment from Waiting to Started.
#' If the experiment has autostart set, this will happen automatically
#' when N subjects have connected. 
#' When the experiment starts, all subjects are moved to the first stage.
#' @param experiment Object of class Experiment
#' @param force Start the experiment even if there are fewer than N participants
#' @details 
#' Note that \code{start} is an S3 generic to avoid clashes with \code{start} in
#' the stats package.
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @method start Experiment
#' @examples
#' start(expt)
#' @export
start.Experiment <- function(experiment, force=FALSE) experiment$handle_command("start",
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

#' Move some clients forward one stage
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
next_stage <- function(experiment, subjid) {
  warning("Moving subjects on manually")
  experiment$handle_command("next_stage", list(subj=subjid))
}


#' Halt the experiment, stopping the server
#' 
#' @param experiment Object of class Experiment
#' @param force If TRUE, force the experiment to halt even if participants are
#'        not finished.
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
halt <- function(experiment, force=FALSE) experiment$halt(force)

#' Show basic info about an experiment
#' 
#' \code{info} prints information about the status, including session name,
#' number of stages, number of clients connected and total N, 
#' status (Stopped, Waiting, Started or Paused)
#' and the URL where the experiment is serving.
#' \code{map} shows a map of how subjects are progressing through the stages.
#' \code{get_url} returns the experiment url.
#' \code{nperiods} returns the number of \code{\link{period}}s in the experiment.
#' \code{session_name} returns the experiment session name, 
#' or NA if the experiment status is Stopped.
#' \code{print_stages} prints a list of the stages in the experiment.
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


#' @rdname info
#' @export
nperiods <- function(experiment) experiment$nperiods()

#' @rdname info
#' @export
print_stages <- function(experiment) experiment$print_stages()

#' Merge a data frame with information about experiment subjects
#' @param experiment an object of class Experiment
#' @param data_frame a data frame containing a column 'id'
#' 
#' @return A new data frame produced by \code{\link{merge}} using the 'id' 
#' column, resulting in new columns 'IP', 'client' and 'seat'. 'period', 'stage'
#' and 'status' will not be merged.
#' 
#' @examples
#' expt <- experiment(N=2, port=12345)
#' expt # on the command line, calls info() 
#' 
#' @family command line functions
#' @export
#' @export
merge_subjects <- function(experiment, data_frame) {
  experiment$merge_subjects(data_frame)
}




#' Replay part or all of an experiment
#' 
#' \code{replay} plays back an experiment using records stored on disk.
#' All requests from clients, and all commands issued on the command line,
#' will be replayed. Afterwards the experiment can be continued.
#' 
#' @param experiment an object of class Experiment
#' @param folder which record to use. Default is the current session 
#' or the most recent session found
#' @param speed how long to wait between each command or request
#' @param maxtime replay only 'maxtime' seconds of the original session
#' @param ask ask before replaying each command or request
#' @param live run the replay "live", with the web server continuing to serve
#' @param clients (character vector) only replay requests from \code{clients}
#' 
#' @details
#' betr records requests and commands in a folder named <experiment
#' name>-YYYY-MM-DD-HHMMSS, where the date and time record when
#' \code{\link{ready}} was called. Within this folder, the subfolder 'record'
#' holds details. If \code{folder} is not given, the default is either the
#' current session name, or the most recently accessed folder matching the 
#' format above. 
#' 
#' Each file in the folder records a single command or request. The filename
#' records the time after experiment start that the command/request was processed.
#' Details are stored in the file using the YAML format, which is quite 
#' self-explanatory and easy to edit.
#' 
#' Unless \code{live=TRUE}, the experiment will be stopped before replay(if it is already started). In any case, the
#' subject table will be reinitialized and \code{\link{ready}} will be called. 
#' This has the effect of calling any user-defined \code{on_ready} function.
#' 
#' \code{speed} can be numeric or "realtime". A numeric gives the number of
#' seconds to wait before executing each command or request. "realtime" means,
#' go at the speed of the original session. \code{live=TRUE} implies
#' \code{speed="realtime"} unless \code{speed} is explicitly set.
#' 
#' If \code{maxtime} is given, only the first \code{maxtime} seconds of the experiment will be replayed.
#' This is useful if you want to "rewind" the experiment because of lab problems,
#' or during debugging.
#' 
#' If \code{ask} is \code{TRUE} then the experimenter will be prompted before each
#' command or request is replayed:
#' 
#' \itemize{
#'   \item enter or 'n' replays the command/request
#'   \item 'c' continues replaying to the end, without prompting again
#'   \item 's' skips the command/request
#'   \item 'q' skips this and all other commands/requests
#'   \item 'd' shows details of the command/request
#'   \item '?' or 'h' shows a help message
#'   \item Any other item will be evaluated in the global environment
#' } 
#' 
#' Note that replay creates a new session. This means that you cannot do
#' 
#' \code{
#' replay(expt, maxtime=30)
#' replay(expt, maxtime=120)
#' }
#' 
#' : the first replay will have created a new session with only the commands 
#' from the first 30 seconds. If you want to move backward and forward
#' within a session, use \code{replay(expt, folder="xxx")} where xxx is the specific 
#' session of interest.
#' 
#' @examples
#' \dontrun{
#' start(expt)
#' # something goes wrong after 2 minutes
#' replay(expt, maxTime=120)
#' }
#' @family command line functions
#' @export
replay <- function(experiment, folder=NULL, maxtime=Inf, speed=NULL, ask=FALSE,
      live=FALSE, clients=NULL) {
  experiment$replay(folder=folder, maxtime=maxtime, speed=speed, ask=ask, 
        clients=clients, live=live)
}
#' Trace one or more experiment stages
#' @param experiment an object of class Experiment
#' @param num numbers of stages to trace
#' @param ... arguments passed on to the \code{trace} method
#'        of the Stage object. See \code{\link{setRefClass}}.
#' 
#' @examples
#' \dontrun{
#' trace_stage(expt, s1, browser)
#' }
#' @family command line functions
#' @export
trace_stage <- function(experiment, num, ...) for (n in num) experiment$stages[[n]]$trace("handle_request", ...)

#' @rdname trace_stage
#' @export
untrace_stage <- function(experiment, num) for (n in num) experiment$stages[[n]]$untrace()
