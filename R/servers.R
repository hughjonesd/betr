
library(svSocket)
library(Rook)

Server <- setRefClass("Server",
  fields=list(
    clients="data.frame",
    auth="ANY",
    pass_request="function"
  ),
  methods=list(
    initialize=function(auth=TRUE, ...) {
      clients <<- data.frame(name=character(0), id=numeric(0), 
            stringsAsFactors=FALSE)
      callSuper(auth=auth, ...)
    },
# broke WRT inheritanc...    
#    finalize = function() {
#      .self$halt()
#    },
    start = function() stop("start() called on abstract class Server"),
    halt = function() stop("halt() called on abstract class Server"),
    .pass_request = function(name, params) pass_request(name, params),
    authorize_and_add = function(name, params){
      # name could be an IP address or whatever: server-specific
      if (name %in% clients$name) return(TRUE)
      ok <- switch(class(auth),
        logical = auth,
        character = any(sapply(auth, grepl, x=name)),
        "function" = auth(name, params),
        stop("auth is of class '", class(auth), "', should be character or function")
      )
      if (ok) add_client(name)
      return(ok)
    },
    add_client = function(name) {
      if (name %in% clients$name) {
        warning("Client ", name, " already exists in clients table")
        return()
      }      
      id <- if (nrow(clients) > 0) max(clients$id) + 1 else 1
      clients <<- rbind(clients, data.frame(name=name, id=id,
            stringsAsFactors=FALSE))
    }
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
      if (authorize_and_add(name, params)) {
        return(.pass_request(name, params))     
      } else {
        return("UNAUTHORIZED")
      }
    }
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
    rhttpd = "Rhttpd",
    unique_id = "character",
    port = "numeric"
  ),
  methods=list(
    initialize = function(port=35538, pass_request=NULL, ...) {
      unique_id <<- paste0("betr-rook-", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
      callSuper(port=port, pass_request=pass_request, ...)
    },
    call = function (env) {
      req <- Rook::Request$new(env)
      res <- Rook::Response$new()
      if (unique_id %in% names(req$cookies())) {
        name <- req$cookies()[[unique_id]]
      } else {
        name <- paste0(req$ip(), "-", do.call(paste0, as.list(sample(LETTERS, 10))))
      }
      params <- req$params()
      if (authorize_and_add(name, params)) {
        res$set_cookie(unique_id, name) # any harm in calling this always?
        # not sure when it expires ^^^
#         Rook::Utils$set_cookie_header(res$header, key=unique_id, value=name,
#               expires=Sys.time() + 3600*24*7)
        html <- .pass_request(name, params)
        res$write(html)
      } else {
        # TODO.
        res$write("UNAUTHORIZED")
      }
      res$finish()
    },
    start = function () {
      if (is.null(rhttpd)) {
        try({
          startDynamicHelp(FALSE) # RStudio hack
          options(help.ports=port)
          startDynamicHelp(TRUE)
        }, silent=TRUE)
        rhttpd <<- Rhttpd$new()
      }
      rhttpd$add(.self, name="betr") # dupes ignored
      rhttpd$start()
    },
    halt = function () {
      # rhttpd$remove("betr") # needed?
      try({
        rhttpd$stop() # error from startDynamicHelp  
      }, silent=TRUE)
      
    }
  )
)

library(testthat)
test_that("Command line server works",{
  cls <- CommandLineServer$new(port=12345, auth="test.*", 
        pass_request = function(name, params) return(params["foo"]))
  expect_that(cls$port, equals(12345))
  expect_that(cls$auth, equals("test.*"))
  expect_that(cls$authorize_and_add("fail", c(a=1,b=2)), is_false())
  expect_that(cls$authorize_and_add("test", c(a=1,b=2)), is_true())
  expect_that(cls$.process_socket("fail foo:bar", "blah", 12345), equals("UNAUTHORIZED"))
  expect_that(cls$.process_socket("test foo:bar", "blah", 12345), matches("bar"))
  cls$auth <- function(name, params) params["password"]=="12345"
  expect_that(cls$.process_socket("whatever password:bad foo:bar"), matches("UNAUTHORIZED"))
  expect_that(cls$.process_socket("whatever password:12345 foo:bar"), matches("bar"))
  expect_that(cls$.process_socket("whatever now:authed foo:baz"), matches("baz"))
  cls$auth <- TRUE
  expect_that(cls$.process_socket("new foo:bar"), matches("bar"))
  cls$auth <- FALSE
  expect_that(cls$.process_socket("new2 foo:bar"), matches("UNAUTHORIZED"))
  
  cls$halt()
})

test_that("Command line server works remotely", {
  cls <- CommandLineServer$new(port=12345, auth="test.*", 
        pass_request = function(name, params) return(params["foo"]))
  cls$start()
  tmp <- socketConnection(port=12345)
  cat("fail foo:bar\n", file=tmp)
  Sys.sleep(0.2)
  resp <- readLines(tmp)
  expect_that(resp, equals("UNAUTHORIZED"))
  cat("test foo:bar\n", file=tmp)
  Sys.sleep(0.2)
  resp <- readLines(tmp)
  expect_that(resp, matches("bar"))
  cls$halt()
  close(tmp)
  expect_that(socketConnection(port=12345), gives_warning())
})

test_that("RookServer works", {
  rs <- RookServer$new(auth=TRUE, 
    pass_request = function(name, params) return(params["foo"]))
  rs$start()
  fake_env <- new.env()
  fake_env$QUERY_STRING="foo=bar"
  fake_env$rook.input = "faked"
  expect_that(rs$call(fake_env)$body, equals("bar"))
  rs$halt()
})
