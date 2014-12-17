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
    initialize = function(..., auth=TRUE, host="127.0.0.1", port=35538, 
      autostart=FALSE, 
      allow_latecomers=FALSE, N=Inf, server="RookServer", name="betr", 
      client_refresh=5, clients_in_url=FALSE, seats_file="betr-SEATS.txt",
      on_ready=NULL, randomize_ids=TRUE, record=TRUE, seed=NULL) {
      stages <<- list()
      initialize_subjects()
      status <<- "Stopped"
      
      # server can be a class name, a class object (refObjectGenerator), 
      # or an actual Server object
      if (inherits(server, "Server")) {
        server$pass_request <<- .self$handle_request 
      }  else {
        server_args <- list(pass_request=.self$handle_request, 
              clients_in_url=clients_in_url, name=name, host=host, port=port)
        sclass <- if (is.character(server)) get(server) else server
        server <<- do.call(sclass$new, server_args)
      }
      requests <<- commands <<- list()
      .command_names <<- c("start", "pause", "restart", "next_stage")
      seats <<- data.frame(seat=numeric(0), IP=character(0), cookie=character(0))
      if (! is.null(seats_file) && nzchar(seats_file)) {
        try(seats <<- read.table(seats_file, header=TRUE, 
              colClasses=c("integer", "character", "character")), 
              silent=TRUE)    
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
            stage=numeric(0), hits=numeric(0), stringsAsFactors=FALSE)
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
      paste0(header(refresh=client_refresh), "<div style='font-size: large;
            font-family: Verdana, sans-serif;'>", message, "</div>", footer())
    },
    
    special_page = function(message) {
      paste0(header(refresh=client_refresh), "<div style='font-size: large;
            font-family: Verdana, sans-serif;'>", message, "</div>", footer())
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
        if (! is.null(cookies) && "betr-seat" %in% names(cookies)) {
          seat <- seats$seat[ seats$cookie==cookies[["betr-seat"]] ] 
        } else if (! is.na(ip)) {
          seat <- seats$seat[seats$IP==ip] 
        } 
        if (length(seat)==0 || is.na(seat)) {
          warning("Seat not found for client with IP address: ",
              sQuote(ip), if (! is.null(cookies)) paste0(", cookie: ", 
              sQuote(cookies[["betr-seat"]])))
          seat <- NA
        }  
      }
      subjects <<- rbind(subjects, data.frame(client=client, IP=ip, id=id, 
            seat=seat, period=0, status=factor("Running", 
            levels=c("Running", "Waiting", "Finished")), stage=0, hits=0,
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
      subjects$hits[srows]   <<- 0
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
      cat(as.yaml(command), file=file.path("betr-data", session_name, "record", paste0("command-", 
            as.character(tm), "-", length(commands))))
    },
    
    record_request = function(client, params, ip=NULL, cookies=NULL) {
      request <- list(client=client, params=params, ip=ip, cookies=cookies)
      requests <<- append(requests, request)
      tm <- elapsed_time()
      cat(as.yaml(request), file=file.path("betr-data", session_name, "record", 
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
          subjects$hits[subjects$id==subject$id] <<- subjects$hits[
                subjects$id==subject$id] + 1
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
      cat(sprintf(
            "%s\tStatus: %s\tElapsed time:%0.1f\nClients: %d/%0.0f\tPeriods: %d\tStages: %d\n", 
            ifelse(status=="Stopped", paste("Name:", name), paste("Session:",
            session_name)), 
            status, 
            ifelse(status=="Stopped", NA, elapsed_time()), 
            nrow(subjects), N, nperiods(), length(stages)))
      if (status != "Stopped") server$info()
      if (subj && nrow(subjects) > 0) {
        cat("Subjects:\n")
        print(cbind(subjects[order(subjects$id),], 
              desc=sapply(subjects$stage[order(subjects$id)], 
              function(x) if (x==0) "" else description(stages[[x]]))))
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
        if (i %in% names(tbl)) cat(i,": ", rep("+", tbl[[i]]), " [", tbl[[i]], "]\n",
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
        dir.create(fp <- file.path("betr-data", session_name, "record"),
              recursive=TRUE)
        if (file.access(fp, 2) != 0) stop("Could not write into ", fp)
        cat(seed, file=file.path("betr-data", session_name, "seed"))
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
          live=FALSE, clients=NULL, rewind=FALSE) {
      if (live && (missing(speed) || is.null(speed))) speed="realtime"
      # if folder is null, use session_name or guess the most recently modified
      if (is.null(folder)) {
        if (length(.self$session_name) == 0) {
          dirs <- file.info(list.files("betr-data", pattern=paste0("^",name, 
                "-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{6}$"), include.dirs=TRUE))
          dirs <- dirs[order(dirs$mtime, decreasing=TRUE),]
          dirs <- dirs[dirs$isdir,]
          folder <- rownames(dirs[1,])
        } else folder <- file.path("betr-data", .self$session_name)
      }
      if (length(folder)==0) stop("Can't find a session to replay!")
      
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
      server <<- ReplayServer$new(folder=folder, experiment=.self,
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
      # we have now created a new session. But we may want to pretend to be
      # the old one.
      if (rewind) {
        file.rename(folder, paste0(folder, ".old"))
        new_folder <- session_name
        session_name <<- folder
        file.rename(new_folder, session_name)
      }
      if (! live) server$start()
      # hopefully we are now back at the approp. period and with correct status...
      # need to think, how could we mix manual and automated clients...      
    }
  )
)
