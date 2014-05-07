#' @import Rook
#' @import svSocket


if (getRversion() < "2.15.0") paste0 <- function(...) paste(..., sep="")

#' @export
Server <- setRefClass("Server",
  fields=list(
    pass_request="function",
    clients_in_url="logical",
    name="character",
    session_name = "character"
  ),
  methods=list(
    initialize = function(clients_in_url=FALSE, ...) {
      callSuper(clients_in_url=clients_in_url, ...)
    },
    
    start = function() stop("start() called on abstract class Server"),
    
    halt = function() stop("halt() called on abstract class Server"),
    
    get_url = function() stop("get_url() called on abstract class Server"),
    
    .pass_request = function(name, params) pass_request(name, params)
  )
)

#' @export
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

#' @export
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
      
      if (session_name %in% names(req$cookies())) {
        client <- req$cookies()[[session_name]]
      } else {
        ip <- req$ip()
        if (length(ip)==0) ip <- "127.0.0.1" # work around Rook bug
        client <- paste0(ip, "-", do.call(paste0, as.list(
              sample(LETTERS, 10))))
      }
      if (clients_in_url) {
        # always overrides cookie
        poss_client <- sub(paste0(".*", name, "/(.*)"), "\\1", req$path())
        if (nchar(poss_client)>0) client <- poss_client
      }    
      params <- req$params()
      
      result <- .pass_request(client, params)
      if (inherits(result, "Response")) {
        result$set_cookie(session_name, client)
        result$finish()
      }
      else {
        res <- Rook::Response$new()
        res$set_cookie(session_name, client)
        res$write(result)
        res$finish()
      }
    },
    start = function (session_name=paste0(name, Sys.time())) {
      session_name <<- session_name
      if (is.null(rhttpd)) {
#         try({
#           startDynamicHelp(FALSE) # RStudio hack
#           options(help.ports=port)
#           #startDynamicHelp(TRUE)
#         }, silent=TRUE)
        rhttpd <<- Rhttpd$new()
      }
      rhttpd$add(app=.self, name=name) # dupes ignored
      rhttpd$start(port=port)
    },
    
    halt = function () {
      # rhttpd$remove("betr") # needed?
      try(rhttpd$stop(), silent=TRUE) # error from startDynamicHelp  
    },
    
    info = function() cat("Serving at", rhttpd$full_url(1), "\n"),
    
    get_url = function() return(rhttpd$full_url(1))
  )
)

#' @export
ReplayServer <- setRefClass("ReplayServer", contains="Server",
  fields=list(
    folder="character",
    speed="ANY",
    maxtime="numeric",
    pass_command="function",
    ask="logical"
  ),
  methods=list(
    initialize = function(pass_request=NULL, folder=NULL, speed=NULL, maxtime=Inf, 
      pass_command=NULL, ask=FALSE, ...) {
       callSuper(folder=folder, speed=speed, maxtime=maxtime, pass_request=pass_request,
         pass_command=pass_command, ask=ask, ...)
    },
    
    start = function(session_name=NULL) {
# browser()
      comreq <- list.files(file.path(folder, "record"), 
        pattern="(command|request)-[0-9\\.]+")
      if (length(comreq) == 0) stop("Found no commands or requests in ", file.path(folder, "record"))
      comreq <- data.frame(name=comreq, type=sub("(command|request).*", "\\1", comreq),
        time=sub("(command|request)-([0-9\\.]+)", "\\2", comreq), command_name=NA, client=NA,
        stringsAsFactors=FALSE)
      paramlist <- list()
      comreq$time <- as.numeric(comreq$time)
      comreq <- comreq[order(comreq$time),]
      comreq <- comreq[comreq$time <= maxtime,]
      for (i in 1:nrow(comreq)) {
        txt <- readLines(file.path(folder, "record", comreq$name[i]), warn=FALSE)
        txt <- sub("^\\s+", "", txt)
        txt <- sub("\\s+$", "", txt)
        if (comreq$type[i]=="command") comreq$command_name[i] <- txt[1] else
          comreq$client[i] <- txt[1]
        paramlist[[i]] <- if (length(txt)>1) tail(txt, -1) else NA
      }
            
      reltimes <- diff(c(0, comreq$time))
      skip <- FALSE
      for (i in 1:nrow(comreq)) {
        # this unfortunately won't let you do anything on command line! or will it...
        if (! is.null(speed)) { 
          if(speed=="realtime") Sys.sleep(reltimes[i])
          if (is.numeric(speed)) Sys.sleep(speed)
        }
        fields <- paramlist[[i]]
        pnames <- sub("([^:]*):.*", "\\1", fields)
        params <- sub("[^:]*:(.*)", "\\1", fields)
        names(params) <- pnames
        params <- as.list(params)
        if (ask) {
          r <- "xxx"
          skip <- FALSE
          while (! r %in% c("y", "", "c", "q", "n")) {
            r <- readline("Next command? ([y]es, [n]o, [c]ontinue to end, [q]uit, [d]etails)\n")
            switch(r, n={skip <- TRUE}, c={ask <<- FALSE}, q={skip <- TRUE; ask <<- FALSE}, d={
                if (comreq$type[i]=="request") cat("Request from client:", comreq$client[i]) else
                  cat("Command:", comreq$command_name[i])
                cat("\nTime from start:", comreq$time[i], "\n")
                cat("Params:\n")
                cat(str(params), "\n")
              },
              ...={
                try(eval(parse(text=r), envir = globalenv()))
              }
            )
          }
        }
        if (skip) next
        switch(comreq$type[i], 
          command= pass_command(comreq$command_name[i], params),
          request= .pass_request(comreq$client[i], params)
        ) 
      }
    },
    
    halt = function() {
      
    },
    
    get_url = function() {
      return(normalizePath(folder))
    }
  )
)
  