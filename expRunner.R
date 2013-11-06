
library(Rook)
WAITING <- "Waiting"
RUNNING <- "Running"
PAUSED <- "Paused"
STOPPED <- "Stopped"

# client statuses
READY <- "Ready"
STARTED <- "Started"
COMPLETED <- "Completed"

# TODO:
# - librarify
# - different stages for different treatments? or equiv of "participate"?
# - think if stage architecture can be simplified
# - consider S3 class for stages? 
# - subjects and clients must be refClass, or at least,
# editable in-place by stages.
#   - maybe they exist in some "global" environment which is in the
#     stage functions' search path?
# - setGeneric, setMethod to allow standard R syntax
# - serveWaitingPage needs to be flexible
# - mechanism for messages to user
# - translations using gettext
# - error handling
# - function to move on client(s) from a stage:
#     - should by default refuse unless COMPLETED
#     - but overrides after() mechanism
#     - and can override COMPLETED, setting COMPLETED and moving on

make_unique_id <- function () paste0(
  sample(c(LETTERS,letters,0:9), 20, replace=TRUE), collapse="")

Experiment <- setRefClass(
  "Experiment",
  fields = list(
    .stages       = "list",
    .setupPeriods  = "numeric",
    .finishedPeriod = "numeric",
    .subjects     = "data.frame",
    .clients      = "data.frame", # user IDs, current period, cookies, IP?
    .status       = "character",
    port          = "numeric",
    server        = "ANY",
    N             = "numeric",
    auth          = "ANY",
    .cur_id       = "integer",
    name          = "character",
    session_name  = "character",
    refresh       = "numeric"
  ),
  
  methods = list(
    initialize = function (port=35538L, N, auth, refresh=10, ...) {
      if (missing(N)) stop("Need to specify N")
      N <<- N
      if (missing(auth)) stop("Need to specify auth")
      auth <<- auth
      .status <<- STOPPED
      .stages <<- list()
      .clients <<- data.frame(id=integer(), cookie=character(), 
            ip=character(), period=integer(), status=character())
      port <<- port
      session_name <<- format(Sys.time(), "%Y-%m-%d_%H%M%S")
      if (! hasArg(server)) server <<- NULL
      callSuper(...)
    },
    
    finalize = function () {
      # write data, assuming this is an emergency stop!
      if (! is.null(server)) server$stop()
      server <<- NULL
    },
            
    status = function () {.status},
    subjects = function () {.subjects},
    clients = function () {.clients},
    cur_id = function () {.cur_id},
    
    info = function () {
      info <- c("Session: ", session_name, "\tStatus: ", .self$status(), 
        "\tClients: ",
        nrow(.clients), "/", N)
      if (.status %in% c(RUNNING, PAUSED) && length(na.omit(.clients$period)) > 0) {
        cp <- .clients$period
        info <- c(info, "\nPeriods:")
        steml <- sapply(min(cp):max(cp),
          function (x) do.call(paste0, list(...=rep("*",sum(x==cp)), collapse="")))
        info <- c(info, paste(steml, "\n"))
      }
      cat(do.call(paste0, as.list(info)))
    },
    
    addStages = function (..., after=1+length(.stages)) {
      .stages <<- append(.stages, ..., after)
      for (st in .stages) {
        st$.add(.self)
      }
    },
    
    moveOn = function (ids=1:N, periods=1:length(.stages), incomplete="skip") {
      cls <- .clients[.clients$id %in% ids & .clients$period %in% periods,]
      ids <- cls$id
      inco <- cls$id[cls$status != COMPLETED]
      if (length(inco)) {
        switch (incomplete,
          force=warning("Forcing non-completed clients ", 
                do.call(paste,list(inco, sep=", ")), " to move on"),
          skip={
            warning("Skipping non-completed clients ", 
              do.call(paste,list(inco, sep=", ")))
            ids <- setdiff(ids, inco)
          },
          fail={
            warning("Clients ", do.call(paste,list(inco, sep=", ")), 
              " are not complete; not moving anyone on")
            return(FALSE)
          }
        )
      }
      .clients$status[.clients$id %in% ids] <<- COMPLETED
      # XXX todo - how to move clients on from static page?
      # refresh (+ some kind of resource unchanged http status?)
    },
    
    cookie_name = function() paste0("expRunner_", session_name),
    
    serveSpecialPage = function (res, message) {
      res$write(paste("<html><body><h1>", message , "</h1></body></html>"))
      res$finish()
    },
  
    serveWaitingPage = function (res, message) {
      res$header("Refresh", as.character(refresh))
      res$write(paste("<html><body><h1>", message , "</h1></body></html>"))
      res$finish()
    },
    
    assignSubjectIDs = function() {
      stopifnot(N==nrow(.clients))
      .clients$id <<- sample.int(N) # do we ever want to do something different?
    },
    
    call = function (env) {
      req <- Rook::Request$new(env)
      res <- MyResp$new()
      
      # is client authenticated (in subjects table)?
      req$cookies() # workaround Request bug
      if (! cookie_name() %in% names(req$cookies())) {
        #  is server accepting clients (WAITING)
        if (.status==WAITING && nrow(.clients) < N) {
          # is client allowed to take part?
          authed <- NA
          if (is.character(auth)) authed <- req$ip() %in% auth
          if (is.function(auth)) authed <- auth(req)
          if (is.null(auth) || is.na(auth)) authed <- TRUE
          if (is.na(authed)) warning("Can't determine authorization")
          if (authed) {
            cid <- make_unique_id()
            res$set_cookie(cookie_name(), cid)
            ip <- req$ip()
            if (is.null(ip)) ip <- "127.0.0.1"
            .clients <<- rbind(.clients, 
                data.frame(id=NA, cookie=cid, period=NA, ip=ip))
            if (nrow(.clients) == N) {
              .clients$period <<- 1
              assignSubjectIDs()
              .status <<- RUNNING
            }
            # this is necessary to get the cookie to the client!
            serveWaitingPage(res, "Waiting for experiment to begin") 
          } else {
            serveSpecialPage(res, "Unauthorized")
          }
        } else {
          serveSpecialPage(res, "Experiment full") 
        }
      } else {
        cid <- req$cookies()[[cookie_name()]]
        id <- .clients$id[.clients$cookie==cid]
        if (! length(id)) stop("Could not find subject id from cookie id", cid)
        #  Yes -> is server WAITING, RUNNING or PAUSED
        switch(.status,
          "Waiting" = serveWaitingPage(res, "Waiting for experiment to begin"),
          "Paused" = serveWaitingPage(res, "Waiting for experiment to continue"),
          "Running" = runCurrentStage(req, res, id)
        )
      }
    },
    
    runCurrentStage = function(req, res, id) {
      period <- .clients$period[.clients$id==id]
      stage <- .stages[[period]]
      if (! period %in% .setupPeriods) {
        stage$.setup()
        .setupPeriods <<- c(.setupPeriods, period)
      }
      
      # if subject status is READY:
      #   - run before, then run html; status <<- STARTED
      st <- .clients$status[.clients$id==id]
      switch(st,
        READY={
          stage$.before(req)
          .clients$status[.clients$id==id] <<- STARTED
          stage$.html(req, res)
        },
        STARTED={
          if (stage$.check(req)) {
            .clients$status[.clients$id==id] <<- COMPLETED
            runCurrentStage(req, res, id)
          } else {
            # error messages?
            stage$.html(req, res)
          }
        },
        COMPLETED={
          if (stage$.after(req)) {
            # move on to next stage
            .clients$status[.clients$id==id] <<- READY
            p <- .clients$period[.clients$id==id]
            .clients$period[.clients$id==id] <<- p + 1
            if (.finishedPeriod <= p && all(.clients$period > p)) {
              stage$finish()
              .finishedPeriod <<- period
            }
            runCurrentStage(req, res, id)
          } else {
            serveWaitingPage("Waiting for experiment to continue")
          }
        }
      )
    },
    
    START = function () {
      if (is.null(server)) {
        try({
          startDynamicHelp(FALSE) # RStudio hack
          options(help.ports=port)
          startDynamicHelp(TRUE)
        }, silent=TRUE)
        server <<- Rhttpd$new()
      }
      server$add(.self, name="expRunner") # dupes ignored
      server$start()
      .status <<- WAITING
    },
    
    browse = function () {
      if (.status %in% c(WAITING, RUNNING, PAUSED)) {
        server$browse("expRunner")
      } else {
        warning("Experiment stopped, can't browse")
      }
    },
    
    STOP = function () {
      if (.status %in% c(PAUSED, RUNNING)) server$stop()
      .status <<- STOPPED
    },
    
    PAUSE = function () {
      .status <<- PAUSED
    }
  )
)

brewpage <- function (filename) {
  fn <- function (req, res) {
    brew(filename)
  }
  return(fn)
}

Stage <- setRefClass("Stage",
  fields=list(
    name    = "character",
    setup   = "function", # called when the first user enters a stage
    before  = "function", # called whenever a user enters a stage
    html    = "ANY", # prints the HTML and headers to res
    check   = "function", # called after HTML shown. Returns TRUE if user can move on. False if need to see html again, perhaps with error.
    after   = "function", # called whenever check passes. Returns TRUE if user can start new stage; otherwise user waits, recalling until true.
    finish  = "function", # called straight after last user leaves a stage
    .expt   = "ANY" # should be Experiment, when I can unbreak this
  ),
  methods=list(
    initialize = function(name, setup, before, html, check, after, finish, waitFor, ...) {
      if (missing(html)) stop("No html method specified for stage")
      setup <<- setup
      before <<- before
      html <<- html
      check <<- check
      after <<- after
      finish <<- finish
      name <<- name
      callSuper(...)
    },
    
    .add = function(expt) {
      .expt <<- expt
    },
    
    .remove = function() {
      .expt <<- NA
    },
    
    .prepareEnv = function(fn) {
      assign("subjects", .expt$subjects(), envir=environment(fn))
      assign("clients", .expt$clients(), envir=environment(fn))
      assign("cur_id", .expt$cur_id(), envir=environment(fn))
      assign("period", XXX, envir=environment(fn))
      assign("stage", .subjects, envir=environment(fn))
    },
    
    .setup = function() {
      .prepareEnv(setup)
      setup()
    },
    
    .before = function(req) {
      .prepareEnv(before)
      before()
    },
    
    .check = function(req) {
      .prepareEnv(check)
      check(req)
    },
    
    .after = function(req) {
      .prepareEnv(after)
      after(req)
    },
    
    .finish = function() {
      .prepareEnv(finish)
      finish()
    },
    
    .html = function(req, res) {
      # case for inheritance?
      if (is.function(html)) {
        .prepareEnv(html)
        html(req, res)
        res$finish() # what if we call this twice?
      } else if (is.character(html)) {
        res$write(html)
        res$finish()
      } else if (inherits(html, "file")) {
        open(html, "r")
        res$write(readLines(html))
        close(html)
        res$finish()
      }
    },
    
    isComplete = function(completedIds) {
      all(waitFor %in% completedIds)
    }
  )
)


# browseURL(paste0("http://localhost:", expt$port, "/custom/expRunner"))