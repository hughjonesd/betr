

if (getRversion() < "2.15.0") paste0 <- function(...) paste(..., sep="")

Server <- setRefClass("Server",
  fields=list(
    pass_request="function"
  ),
  methods=list(
    start = function() stop("start() called on abstract class Server"),
    halt = function() stop("halt() called on abstract class Server"),
    .pass_request = function(name, params) pass_request(name, params)
  )
)

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
    start = function() {
      startSocketServer(port=port, procfun=.self$.process_socket)
    },
    halt = function() {
      stopSocketServer(port=port)
    },
    .process_socket = function(msg, socket, server_port, ...) {
      fields <- strsplit(msg, " ", fixed=TRUE)[[1]]
      name <- fields[1]
      pnames <- sub("([^:]*):.*", "\\1", fields[-1])
      params <- sub("[^:]*:(.*)", "\\1", fields[-1])
      names(params) <- pnames
      return(.pass_request(name, params))     
    },
    info = function() cat("Listening on port ", port, "\n")
  )
)


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

RookServer <- setRefClass("RookServer", contains="Server",
  fields=list(
    rhttpd = "FixedRhttpd",
    unique_id = "character",
    port = "numeric",
    client_param = "character"
  ),
  methods=list(
    initialize = function(port=35538, pass_request=NULL, client_param="",...) {
      unique_id <<- paste0("betr-rook-", format(Sys.time(), 
            "%Y-%m-%d_%H%M%S"))
      callSuper(port=port, pass_request=pass_request, client_param=client_param,
            ...)
    },
    finalize = function() halt(),
    call = function (env) {
      req <- Rook::Request$new(env)
      res <- Rook::Response$new()
      if (unique_id %in% names(req$cookies())) {
        client <- req$cookies()[[unique_id]]
      } else {
        ip <- req$ip()
        if (length(ip)==0) ip <- "127.0.0.1" # work around Rook bug
        client <- paste0(ip, "-", do.call(paste0, as.list(
              sample(LETTERS, 10))))
      }
      
      params <- req$params()
      if (nchar(client_param)>0 && client_param %in% names(req$params())) {
        client <- params[[client_param]]
        params[[client_param]] <- NULL
      }
      html <- .pass_request(client, params)
      res$set_cookie(unique_id, client)
      res$write(html)
      res$finish()
    },
    start = function () {
      if (is.null(rhttpd)) {
#         try({
#           startDynamicHelp(FALSE) # RStudio hack
#           options(help.ports=port)
#           #startDynamicHelp(TRUE)
#         }, silent=TRUE)
        rhttpd <<- FixedRhttpd$new()
      }
      rhttpd$add(.self, name="betr") # dupes ignored
      rhttpd$start(port=port)
    },
    halt = function () {
      # rhttpd$remove("betr") # needed?
      try(rhttpd$stop(), silent=TRUE) # error from startDynamicHelp  
    },
    info = function() cat("Serving at", rhttpd$full_url(1), "\n")
  )
)
