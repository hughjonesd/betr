setwd("~/Dropbox/essexRun/")
source("servers.R")

Experiment <- setRefClass("Experiment",
  fields=list(
    stages="ANY",
    N="numeric",
    session_name="character",
    status=function(x) {
      if(missing(x)) return(.status)
      stopifnot(x %in% c("Stopped", "Waiting", "Started", "Paused"))
      .status <<- x
    },
    .status="character",
    .setup_stages="list",
    autostart="logical",
    allow_latecomers="logical",
    server="Server",
    subjects="data.frame",
    participants="list"
  ),
  methods=list(
    initialize = function(..., auth=TRUE, port, autostart=FALSE, N=Inf, 
          server="RookServer", name="betr") {
      stages <<- list()
      subjects <<- data.frame(id=numeric(0), period=numeric(0))
      participants <<- list()
      status <<- "Stopped"
      session_name <<- paste0(name, "-", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
      # server can be a class name, a class object (refObjectGenerator), 
      # or an actual Server object
      if (! inherits(server, "Server")) {
        server_args <- list(auth=auth, pass_request=.self$handle_request)
        sclass <- if (is.character(server)) get(server) else server
        if (missing(port) && sclass$className %in% c("RookServer", "CommandLineServer"))
              server_args$port <- 35538
        server <<- do.call(sclass$new, server_args)
      }
      
      callSuper(..., autostart=autostart, N=N)
      if (is.infinite(N)) warning("No maximum N set for experiment")
    },
    add_stage = function(..., after=length(stages)) {
      stages <<- append(stages, list(...), after=after)
    }, 
    print = function() {
      # status
      # N total subjects, and number of active subjects
      # number of stages
      # when started
      # what stage subjects are at 
      # delegate some to server?
      # web address  
    },
    ready = function() {
      if (status != "Stopped") {
        warning("Called ready() on an experiment with status: ", status)
        return(invisible(FALSE))
      }
      ## run server
      server$start()
      status <<- "Waiting"
      return(TRUE)
    },
    .create_ids = function() sample(1:length(participants)),
    start = function() {
      if (length(participants) > 0) {
        ids <- .create_ids()
        for (i in 1:length(participants)) participants[[i]]$id <<- ids[i]
        # all participants enter their first stage
        for (part in participants) next_stage(part)
      } else {
        warning("Experiment started with no participants")
      }
      status <<- "Started"
    },
    pause = function() {
      status <<- "Paused"
    },
    add_participant = function(part) {
      # called from server, or directly for tests
      
      # TODO errors must be caught in production mode
      if (length(participants) >= N) stop("Too many participants")
      if (! status %in% c("Started", "Waiting")) stop("Not accepting participants")
      if (status=="Started" && ! latecomers_ok) 
            stop("Tried to add participant but experiment started")
      if (any(sapply(participants, identical, part))) stop("Duplicate participant")
      cids <- sapply(participants, function (x) x$client_id)
      if (anyDuplicated(cids)) stop("Duplicate client IDs")
      participants <<- append(participants, part)
      # if we reach N, trigger a change of state
      if (length(participants)==N && autostart) {
        start()
      }
      handle_request(part) # TODO is this necessary? or even OK?
    },
    next_stage = function(part) {
      part$period <- part$period + 1
      part$status <- "Ready"
      stg <- stages[[part$period]]
      if (! stg$is_setup) stg$.setup()
    },
    handle_request = function(part, params=NULL) {
      # called by the server
      # questions someone must answer, in general:
      # is experiment running? Handled by Experiment.
      # is client authenticated? Handled by Server (tied to e.g. HTTP)
      # is client ready to see HTML? Handled by Server and Stage... ???
      switch(status, 
        Stopped={
          warning("Got handle_request but experiment is stopped")
          # TODO: what gets returned to the server, in general?
        },
        Paused=waiting_page("Experiment paused"),
        Waiting=waiting_page("Waiting to start"),
        Started={
          stage <- stages[[part$period]]
          wait_for <- stage$.wait_for(part) # a list of participants, "all", "none"
          if (is.character(wait_for)) wait_for <- switch(wait_for, 
                all=participants, none=list(part), stop("Stage groups returned '", 
                wait_for, "', should be 'all', 'none' or a list of ids"))
          if (any(sapply(wait_for, function (x) x$period) < part$period)) {
            waiting_page("Waiting for other participants")
          } else {
            stage$handle_request(part, params)
          }
        },
        stop("Unrecognized experiment status:", status)
      )
    },
    waiting_page = function(message="") {
      # TODO
      return(paste0("Please wait:", message))
    },
    subject_finished_stage = function(subj) {
      # experiment lets stage decide when subjects have finished?
    }
  )
)

experiment <- function (...) Experiment$new(...)

# everyone goes through a stage, but they do so at different times
# and possibly waiting for different people to start
# should they also need to wait to finish? Yes, if there is logic in the "after"
Stage <- setRefClass("Stage", 
  fields = list(
    group = "ANY",
    before = "ANY",
    main = "ANY",
    check = "ANY",
    is_setup = "logical",
    setup = "function",
    after = "function",
    finish = "function"
  ),
  methods = list(
    initialize = function(...) {
      is_setup <<- FALSE
      callSuper(...)
    },
    .setup = function () {
      # TODO check environment preparted
      if (is.function(setup)) setup()
      is_setup <<- TRUE
    },
    .wait_for = function(part) {
      if (! exists("group", inherit=FALSE)) return("none")
      switch(class(group),
        "NULL" = return("none"),
        "character" = if (group %in% c("none", "all")) return(group),
        "function" = return(group(part$id)),
        # e.g. list(1:3, 4:6, 7:9) defines groups
        "list" = sapply(group, function (x) if (part$id %in% x) return(x))
      )
      stop("Could not determine who to wait for")
    },
    handle_request = function(part, params) {
      # error if participant is not in right list
      # check whether all relevant subjects are READY. If not, show waiting page
      # (how is this handled?)
      wf <- .wait_for(part)
      # if participant status in stage is READY, run main
      if(part$status=="Ready") return(.main(part, params))
      if (part$status=="Running") {
        if (.check(part, params)) {
          # how do we tell the experiment this part is ready to move on?
        } else {
          return(.main(part, params))  
        }
      }
      stop("Participant status is Finished, shouldn't be calling handle_request")      
    },
    .check = function (part, params) {
      if (! exists("check")) return(TRUE)
      rv <- TRUE
      tryCatch(check(part, params), error= function(e) {
        part$error_message <- e$message
        rv <<- FALSE
      })
      rv
    },
    .main = function (part, params) {
      switch(class(main),
        "function" = main(part, params),
        character  = main,
        stop("main was class '", class(main), "', should be character or function")
      )
    },
    .after = function (part) {
      if (exists("after", inherit=FALSE)) after(part)
    },
    .finish = function(period) {
      if (exists("finish", inherit=FALSE)) finish(period)
    }
  )
)

stage <- function (...) Stage$new(...)
add_stage <- function (expt, ...) expt$add_stage(...)

Participant <- setRefClass("Participant",
  fields=list(
    client_id = "numeric",
    period = "numeric",
    .status = "character",
    status = function (x) {
      if(missing(x)) return(.status)
      stopifnot(x %in% c("Ready", "Running", "Finished"))
      .status <<- x
    },
    id = "numeric",
    error_message = "character"
  ),
  methods=list(
    initialize = function(...) {
      period <<- 0
      callSuper(...)
    }
  )
)

ParticipantGroup <- setRefClass("ParticipantGroup",
  fields=list(
    participants="list"
  ),
  methods=list(
  )
)

library(testthat)
test_that("Experiment stages work", {
  expt <- experiment(N=2, server="CommandLineServer")
  s1 <- stage(main="Stage 1")
  s2 <- stage(main="Stage 2")
  add_stage(expt, s1, s2)
  add_stage(expt, s2, after=1)
  expect_that(length(expt$stages), equals(3))
  expect_that(expt$N, equals(2))
  expect_false(expt$autostart)
  expt$autostart <- TRUE
  expect_that(expt$status <- "B0rked", throws_error())
  
  t1 <- Participant$new()
  expect_that(expt$add_participant(t1), throws_error())
  expt$ready()
  expect_that(expt$status, equals("Waiting"))
  expect_that(expt$add_participant(t1), equals(expt$waiting_page("Waiting to start")))
  expect_that(length(expt$participants), equals(1))
  expect_that(expt$add_participant(t1), throws_error())
  expect_that(expt$add_participant(Participant$new()), equals(s1$.main()))
  expect_that(expt$status, equals("Started"))
})

test_that("Command line server & client work", {
  expt <- Experiment$new(N=1, server=CommandLineServer)
  s1 <- Stage$new(main="Got to stage s1")
  expt$add_stage(s1)
  # TODO add clclient, or is this tested in servers?
})


test_that("Rook server works", {
  expt <- Experiment$new(N=1, server=CommandLineServer)
  s1 <- Stage$new(main="Got to stage s1")
  expt$add_stage(s1)
})