Stage <- setRefClass("Stage", 
  fields=list(
    handle_request = "function"
  ),
  methods=list(
    initialize = function(...) callSuper(handle_request=..1)
  )
)

stage <- function (...) stage$new(...)

NEXT <- -1
WAIT <- -2

Experiment <- setRefClass("Experiment",
  fields=list(
    stages="ANY",
    N="numeric",
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
    start_time="POSIXct"
  ),
  methods=list(
    initialize = function(..., auth=TRUE, port, autostart=FALSE, 
      allow_latecomers=FALSE, N=Inf, server="RookServer", name="betr", 
      client_refresh=10, client_param="") {
      stages <<- list()
      subjects <<- data.frame(client=character(0), id=numeric(0), 
        period=numeric(0), status=factor(, levels=c("Running", "Waiting", 
          "Finished")), stringsAsFactors=FALSE)
      status <<- "Stopped"
      start_time <<- Sys.time()
      session_name <<- paste(name, format(start_time, 
        "%Y-%m-%d-%H%M%S"), sep="-")
      # server can be a class name, a class object (refObjectGenerator), 
      # or an actual Server object
      if (! inherits(server, "Server")) {
        server_args <- list(pass_request=.self$handle_request, 
              client_param=client_param)
        sclass <- if (is.character(server)) get(server) else server
        if (missing(port) && sclass$className %in% 
            c("RookServer", "CommandLineServer"))
          server_args$port <- 35538
        server <<- do.call(sclass$new, server_args)
      }
      requests <<- commands <<- list()
      .command_names <<- c("ready", "start", "pause", "restart", "next_period")
      callSuper(..., auth=auth, autostart=autostart, 
        allow_latecomers=allow_latecomers, N=N, client_refresh=client_refresh)
      if (is.infinite(N)) warning("No maximum N set for experiment")
    },
    
    add_stage = function(..., times, each, after) {
      if (status != "Stopped") 
        warning("Adding stage to server while status is", status, "... this is unwise.")
      stgs <- list()
      for (st in list(...)) stgs <- append(stgs, 
            if (is.function(st)) Stage$new(st) else st)
      if (! missing(times)) stgs <- rep(stgs, times=times)
      if (! missing(each)) stgs <- rep(stgs, each=each)
      if (missing(after)) after <- length(stgs)
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
      subjects <<- rbind(subjects, data.frame(id=id, period=0,
        client=client, status=factor("Running", levels=c("Running", "Waiting", "Finished")), stringsAsFactors=FALSE))
      if (status=="Started") next_period(subjects[subjects$client==client,])
      # if we reach N, trigger a change of state
      if (length(subjects)==N && autostart) {
        start() # not via handle_command; will be autotriggered on replay
      }
      return(subjects[subjects$client==client,])
    },
    
    next_period = function(subj) {
      if (status != "Started") {
        warning("Experiment status is not 'Running', cannot move subjects on")
        return()
      }
      if (is.numeric(subj)) subj <- subjects[subjects$id==subj,]
      srows <- subjects$id %in% subj$id & subjects$period < length(stages)
      subjects$period[srows] <<- subjects$period[srows] + 1
      subjects$status[srows] <<- "Running" # do I need this?
      done <- subjects$id %in% subj$id & subjects$period == length(stages)
      subjects$status[done] <<- "Finished"
    },
    
    record_command = function(command, params) {
      if (missing(params)) params <- NULL
      command <- list(name=command, params=params, 
        time=Sys.time() - start_time)
      commands <<- append(commands, command)
      # TODO: print to file
    },
    
    record_request = function(client, params) {
      request <- list(client=client, params=params, 
        time=Sys.time() - start_time)
      requests <<- append(requests, request)
      # TODO: print to file
    },
    
    handle_command = function(command, params) {      
      if (command %in% .command_names) {
        record_command(command, params) 
        command <- do.call(`$`, list(.self, command)) 
        if (missing(params)) command() else command(params)
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
      return(.handle_request(subject, params))
    },
    
    .handle_request = function(subject, params) {
      switch(status, 
        Stopped = stop("Got handle_request but experiment is stopped"),
        Paused  = waiting_page("Experiment paused"),
        Waiting = waiting_page("Waiting to start"),
        Started = {
          if (subject$status=="Finished") return(special_page("Experiment finished"))
          stage <- stages[[subject$period]]
          result <- stage$handle_request(subject$id, subject$period, params)
          if (result==NEXT) {
            next_period(subject)
            # NB we clean the params when the subject moves on. Is this OK?
            return(.handle_request(subjects[subjects$id==subject$id,]))
          } else if (result==WAIT) {
            subjects$status[subjects$id==subject$id] <<- "Waiting"
            return(waiting_page("Waiting for experiment to continue"))
          } else {
            return(result)
          }
        },
        stop("Unrecognized experiment status:", status)
      )
    },
        
    info = function(subj=FALSE, map=FALSE) {
      cat(sprintf("Session: %s\t\tStatus: %s\t\tClients: %d/%0.0f\t\tStages: %d\n", 
            session_name, status, nrow(subjects), N, length(stages)))
      if (status != "Stopped") server$info()
      if (subj && nrow(subjects) > 0) {
        cat("Subjects:\n")
        print(subjects)
      }
      if (map) .self$map()
    },
    
    map = function() {
      tbl <- table(subjects$period)
      cat("Stage progression:\n")
      for (i in as.character(1:length(stages))) {
        cat(i,":", if (i %in% names(tbl)) paste(rep("*", tbl[[i]]), "[", tbl[[i]], "]", 
              collapse=""), "\n")
      }
    },

    ready = function() {
      if (status != "Stopped") {
        warning("Called ready() on an experiment with status ", status)
        return(invisible(FALSE))
      }
      ## run server
      server$start()
      status <<- "Waiting"
      return(invisible(TRUE))
    },
    
    start = function() {
      if (status != "Waiting") {
        warning("Experiment status is '", status, "' - call ready() first")
        return()
      }
      status <<- "Started"
      if (length(subjects) > 0) {
        next_period(subjects)
      } else {
        warning("Experiment started with no participants")
      }
    },
    
    pause = function() {
      if (status != "Running") {
        warning("Experiment not running, cannot pause")
      } else {
        status <<- "Paused"
      }
    },
    
    restart = function() {
      if (status != "Paused") {
        warning("Experiment not paused, cannot restart")
      } else {
        status <<- "Running"
      }
    }
    
  )
)

print.Experiment <- function(x, ...) x$info() # TODO not working... ;-)
experiment <- function (...) Experiment$new(...)
add_stage <- function (expt, ...) expt$add_stage(...)
start <- function(expt) expt$handle_command("start")
ready <- function(expt) expt$handle_command("ready")
pause <- function(expt) expt$handle_command("pause")
next_period <- function(expt, subjid) {
  warning("Moving subject on manually, this may do bad things to your data")
  expt$handle_command("next_period", list(subjid))
}
info <- function(expt, ...) expt$info(...)
map <- function(expt) expt$map()

