#' @import tools
#' @import Rook
#' @import yaml
#' @import svSocket



if (getRversion() < "2.15.0") paste0 <- function(...) paste(..., sep="")

#' @export Server
#' @exportClass Server
Server <- setRefClass("Server",
  fields=list(
    pass_request="function",
    clients_in_url="logical",
    name="character",
    session_name = "character",
    start_time = "POSIXct"
  ),
  methods=list(
    initialize = function(clients_in_url=FALSE, ...) {
      callSuper(clients_in_url=clients_in_url, ...)
    },
    
    start = function() stop("start() called on abstract class Server"),
    
    halt = function() stop("halt() called on abstract class Server"),
    
    get_url = function() stop("get_url() called on abstract class Server"),
    
    elapsed_time = function() as.numeric(Sys.time() - start_time, units="secs"),
    
    .pass_request = function(name, params, ip=NULL, client=NULL) 
      pass_request(name, params, ip, client)
  )
)

#' @export CommandLineServer
#' @exportClass CommandLineServer
CommandLineServer <- setRefClass("CommandLineServer", 
  contains="Server",
  fields=list(
    port="numeric"
  ),
  methods=list(
    initialize = function(port=35538, pass_request=NULL, ...) {
      callSuper(port=port, pass_request=pass_request, ...)
    },
    
    finalize = function() halt(),
    start = function(session_name=paste0("betr", Sys.time())) {
      start_time <<- Sys.time()
      svSocket::startSocketServer(port=port, procfun=.self$.process_socket)
    },
    
    halt = function() {
      svSocket::stopSocketServer(port=port)
    },
    
    .process_socket = function(msg, socket, server_port, ...) {
      fields <- strsplit(msg, " ", fixed=TRUE)[[1]]
      client <- fields[1]
      pnames <- sub("([^:]*):.*", "\\1", fields[-1])
      params <- sub("[^:]*:(.*)", "\\1", fields[-1])
      names(params) <- pnames
      params <- as.list(params)
      return(.pass_request(client, params))     
    },
    
    info = function() cat("Listening on port ", port, "\n"),
  
    get_url = function() {
      warning("No URL defined for command line server!")
      return(character(0))
    }
  )
)


#' Starts a client to connect to the command line server
#' 
#' Requests can be sent as 
#' "client param:value param:value param:"quoted value"...
#' 
#' @param port port to connect on
#' @export
clclient <- function(port=35538) {
  cat("Enter arguments as 'name param:value param:\"quoted value\"...'\n")
  cat("Enter a period (.) to cancel\n")
  sc <- socketConnection(port=port)
  repeat{
    l <- readline("clclient> ")
    if (l==".") break
    writeLines(l, con=sc)  
    Sys.sleep(0.01)
    cat(readLines(sc))
  }
  close(sc)
}

#' @export RookServer
#' @exportClass RookServer
RookServer <- setRefClass("RookServer", contains="Server",
  fields=list(
    rhttpd = "Rhttpd",
    port = "numeric"
  ),
  methods=list(
    initialize = function(port=35538, pass_request=NULL, ...) {
      callSuper(port=port, pass_request=pass_request, ...)
    },
    
    finalize = function() halt(),
    
    call = function (env) {
      req <- Rook::Request$new(env)
      ip <- req$ip()
      if (length(ip) == 0) {
        # work around Rook bug
        ip <- "127.0.0.1" 
        if (exists("HTTP_X_FORWARDED_FOR", env)) {
          ip <- sub("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}).*",
                "\\1", env$HTTP_X_FORWARDED_FOR)
        }
      }
      cookies <- req$cookies()
      # purpose of 'client' is to set a per-session ID in the browser.
      if ("betr" %in% names(cookies)) client <- cookies$betr else 
            client <- paste0(ip, "-", paste(sample(LETTERS, 10), collapse=''))
      if (clients_in_url) {
        # always overrides cookie
        poss_client <- sub(paste0(".*", name, "/(.*)"), "\\1", req$path())
        if (nchar(poss_client)>0) client <- poss_client
      }    
      params <- req$params()
      # workaround Rook bug:
      if (packageVersion("Rook") <= "1.1.1") names(params)[params==""] <- 
            sub("=$", "", names(params)[params==""])
      result <- .pass_request(client, params, ip, cookies)
         
      if (! inherits(result, "Response")) {
        res <- Rook::Response$new()
      } else {
        res <- result
      }
      Rook::Utils$set_cookie_header(res$headers, "betr", client, 
              expires=as.POSIXct(Sys.time() + 3600*24*365))
      if (! inherits(result, "Response")) res$write(result) 
      res$finish()
    },
    
    start = function (session_name=paste0(name, Sys.time())) {
      session_name <<- session_name
      require(tools) # seems to fix weird Rhttpd$start startDynamicHelp problem
      if (is.null(rhttpd)) rhttpd <<- Rhttpd$new()
      rhttpd$add(app=.self, name=name) # dupes ignored
      rhttpd$start(port=port)
      start_time <<- Sys.time()
    },
    
    halt = function () {
      try(rhttpd$stop(), silent=TRUE) # error from startDynamicHelp  
    },
    
    info = function() cat("Serving at", rhttpd$full_url(1), "\n"),
    
    get_url = function() return(rhttpd$full_url(1))
  )
)

#' @export ReplayServer
#' @exportClass ReplayServer
ReplayServer <- setRefClass("ReplayServer", contains="Server",
  fields=list(
    folder="character",
    speed="ANY",
    maxtime="numeric",
    pass_command="function",
    ask="ANY",
    fake_time="numeric",
    clients="ANY"
  ),
  methods=list(
    initialize = function(pass_request=NULL, folder=NULL, speed=NULL, maxtime=Inf, 
      pass_command=NULL, ask=FALSE, clients=NULL, ...) {
      clients <<- clients
       callSuper(folder=folder, speed=speed, maxtime=maxtime, pass_request=pass_request,
         pass_command=pass_command, ask=ask, ...)
    },
    
    start = function(session_name=NULL) {
      comreq <- list.files(file.path(folder, "record"), 
            pattern="(command|request)-[0-9\\.]+")
      if (length(comreq) == 0) stop("Found no commands or requests in ", 
            file.path(folder, "record"))
      comreq <- data.frame(
            name=comreq, 
            type=sub("(command|request).*", "\\1",comreq),
            time=sub("(command|request)-([0-9\\.]+).*", "\\2", comreq), 
            order=sub(".*-([0-9])", "\\1", comreq), 
            stringsAsFactors=FALSE)
      comreq$time <- as.numeric(comreq$time)
      comreq <- comreq[order(comreq$time),]
      comreq <- comreq[comreq$time <= maxtime,]
      cr.data <- list()
      for (i in 1:nrow(comreq)) {
        if (is.na(comreq$name[i])) browser()
        cr.data[[i]] <- yaml.load_file(file.path(folder, "record", comreq$name[i]))
      }
            
      reltimes <- diff(c(0, comreq$time))
      skip <- FALSE
      fake_time <<- 0
      for (i in 1:nrow(comreq)) {
        # this unfortunately won't let you do anything on command line! or will it...
        if (! is.null(speed)) { 
          if(speed=="realtime") Sys.sleep(reltimes[i])
          if (is.numeric(speed)) Sys.sleep(speed)
        }
        if (! is.null(clients) && ! cr.data[[i]]$client %in% clients) next
        if (isTRUE(ask) || is.numeric(ask) && fake_time > ask) {
          r <- "xxx"
          skip <- FALSE
          while (! r %in% c("n", "", "c", "q", "s")) {
            r <- readline("replay > ")
            switch(r, s={skip <- TRUE}, c={ask <<- FALSE}, 
              q={skip <- TRUE; ask <<- FALSE}, 
              d={
                if (comreq$type[i]=="request") cat("Request from client:", 
                      cr.data[[i]]$client) else cat("Command:", 
                      cr.data[[i]]$name)
                cat("\nTime from start:", comreq$time[i], "\n")
                cat("Params:\n")
                cat(str(cr.data[[i]]$params), "\n")
              },
              h=,
              "?"=cat("[n]ext command/request, [s]kip command/request, [c]ontinue to end, [q]uit, [d]etails, [?h]elp, or enter R expression\n"),
              n=NULL,
              if (nchar(r)>0) try(print(eval(parse(text=r), 
                    envir = globalenv())))
            )
          }
        }
        if (skip) next
        fake_time <<- comreq$time[i]
        switch(comreq$type[i], 
          command= pass_command(cr.data[[i]]$name, cr.data[[i]]$params),
          request= .pass_request(cr.data[[i]]$client, cr.data[[i]]$params, 
                cr.data[[i]]$ip, cr.data[[i]]$cookies)
        ) 
      }
    },

    info = function() cat("Replaying from folder", folder, "\n"),
    
    halt = function() {},
    
    get_url = function() {
      return(normalizePath(folder))
    },
    
    elapsed_time = function() fake_time
  )
)
  
