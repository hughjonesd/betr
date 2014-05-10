
library(testthat)

# test_that("Command line server works remotely", {
#   cls <- CommandLineServer$new(port=12345, 
#     pass_request = function(name, params, ip, client) return(params["foo"]))
#   cls$start()
#   tmp <- socketConnection(port=12345)
#   cat("test foo:bar\n", file=tmp)
#   Sys.sleep(0.2)
#   resp <- readLines(tmp)
#   expect_that(resp, matches("bar"))
#   cls$halt()
#   close(tmp)
#   expect_that(socketConnection(port=12345), gives_warning())
# })

test_that("RookServer works", {
  rs <- RookServer$new(name="jim", clients_in_url=FALSE,
        pass_request = function(name, params, ip, cookies) return(params["foo"]))
  rs$start()
  fake_env <- new.env()
  fake_env$QUERY_STRING="foo=bar"
  fake_env$rook.input = "faked"
  expect_that(rs$call(fake_env)$body, equals("bar"))
  rs$halt()
})

test_that("Experiment stages work", {
  expt <- experiment(N=2, server="CommandLineServer")
  s1 <- function(...) "Stage 1"
  s2 <- stage(handler=function(...) "Stage 2")
  add_stage(expt, s1, s2)
  add_stage(expt, s2, after=1)
  expect_that(length(expt$stages), equals(3))
  add_stage(expt, s1, s2, each=2)
  add_stage(expt, s1, s2, times=2)
  for (i in c(4:5,8)) expect_that(expt$stages[[1]], is_equivalent_to(expt$stages[[i]]))
  for (i in c(6:7,9)) expect_that(s2, is_equivalent_to(expt$stages[[i]]))
})


Sys.sleep(1) # new session name
test_that("Experiment starting and status works", {
  expt <- experiment(N=2, server="CommandLineServer")
  
  expect_that(expt$N, equals(2))
  expect_that(expt$status <- "B0rked", throws_error())
  expect_that(show(expt), prints_text("Stopped"))
  
  ready(expt)  
  expect_that(show(expt), prints_text("Waiting"))
  expect_that(expt$handle_request(client="A", params=list()), 
        equals(expt$waiting_page("Waiting to start")))
  expect_that(nrow(expt$subjects), equals(1))
  expect_that(expt$handle_request(client="A", params=list()), 
       equals(expt$waiting_page("Waiting to start")))
  expect_that(nrow(expt$subjects), equals(1))
  expect_that(start(expt), gives_warning())
  expect_that(show(expt), prints_text("Waiting"))
  expect_that(expt$handle_request(client="B", params=list()), 
        equals(expt$waiting_page("Waiting to start")))
  expect_that(nrow(expt$subjects), equals(2))
  expect_that(show(expt), prints_text("Waiting"))  
  
  start(expt)
  expect_that(info(expt), prints_text("Started"))
  expect_that(show(expt), prints_text("Started"))
  expect_that(expt$handle_request(client="C", params=list()), 
        equals(expt$special_page("Too many participants")))
})


Sys.sleep(1) # new session name
test_that("Command line server & client work", {
  expt <- experiment(N=1, server=CommandLineServer)
  s1 <- stage(handler=function(...) "Got to stage s1")
  add_stage(expt, s1)
  ready(expt)
  expt$handle_request("jim", list())
  start(expt)
  expt$handle_request("jim", list())
  # TODO add clclient, or is this tested in servers?
})


Sys.sleep(1) # new session name
test_that("Rook server works with experiment", {
  expt <- experiment(N=1, server="RookServer", autostart=TRUE)
  s1 <- stage(handler=function(...) "Got to stage s1")
  add_stage(expt, s1)
  ready(expt)
  expect_that(show(expt), prints_text("Waiting"))
  expect_that(expt$handle_request("jim", list()), equals("Got to stage s1"))
  
  expt <- experiment(N=1, server="RookServer", autostart=FALSE)
  add_stage(expt, s1)
  ready(expt)
  expect_that(expt$handle_request("jim", list()), 
        equals(expt$waiting_page("Waiting to start")))
  start(expt)
  expect_that(expt$handle_request("jim", list()), equals("Got to stage s1"))
  expt$handle_request("jim", list())
})

test_that("TextStages work", {
  s1 <- text_stage(text="Foo")
  expect_that(s1$handle_request(1,1, list()), equals("Foo"))
  expect_that(s1$handle_request(2,1, list()), equals("Foo"))
  expect_that(s1$handle_request(2,1, list()), equals(NEXT))
  t1 <- tempfile("text_stage_test", fileext=".html")
  cat("foo\n", file=t1)
  s2 <- text_stage(file=t1)
  expect_that(s2$handle_request(1,1, list()), equals("foo"))
  t2 <- tempfile("text_stage_test", fileext=".brew")
  cat("foo<%= bar %>\n", file=t2)
  bar <<- "boing"
  s3 <- text_stage(file=t2)
  expect_that(s3$handle_request(1,1, list()), equals("fooboing"))
  expect_that(text_stage(file="whatever", text="foo"), throws_error())
})

test_that("StructuredStages work", {
  s1 <- structured_stage(
    form=function(id, period, params, error='') paste("Foo", error),
    process=function(id, period, params) stop("barf!")
  )
  expect_that(s1$handle_request(1,1, list())$body, equals("Foo "))
  expect_that(s1$handle_request(1,1, list()), equals("Foo barf!"), 
        info="Errors in process not being caught")
})

Sys.sleep(1)
test_that("Periods work", {
  init_data <- function() {
    myperiods <<- rep(NA,4) 
    seen <<- matrix(FALSE, nrow=4, ncol=4)
  }
  expt <- experiment(N=4, server="RookServer", autostart=FALSE, on_ready=init_data)
  s1 <- stage(handler=function(id, period, params) {
    myperiods[id] <<- period
    sn <- seen[id, period]
    seen[id, period] <<- TRUE
    if (sn) return(NEXT) else return("some html")
  })
  add_stage(expt, period(), s1, times=2)
  expect_that(nperiods(expt), equals(2))
  add_stage(expt, period("all"), s1)
  add_stage(expt, period(c(1,1,2,2)), s1)
  
  ready(expt)
  expect_that(myperiods, equals(rep(NA, 4)))
  rfrom <- function(cl) expt$handle_request(cl, list())
  clients <- paste0("client", 1:4)
  sapply(clients, rfrom) 
  start(expt)
  
  sapply(clients, rfrom) 
  expect_that(myperiods, equals(rep(1, 4)))
  rfrom(clients[1])
  expect_that(myperiods, equals(c(2,1,1,1)))
  sapply(clients[2:4], rfrom)
  expect_that(myperiods, equals(rep(2,4)))   
  sapply(clients[1:3], rfrom)
  expect_that(myperiods, equals(rep(2,4)))
  rfrom(clients[4]) # client 4 completes the set
  expect_that(myperiods, equals(c(2,2,2,3)))
  sapply(clients[1:3], rfrom) # now 1-3 let through
  expect_that(myperiods, equals(rep(3,4)))

  rfrom(clients[1])
  rfrom(clients[3])
  expect_that(myperiods, equals(rep(3,4)))
  rfrom(clients[2])
  rfrom(clients[1])
  expect_that(myperiods, equals(c(4,4,3,3)))
  rfrom(clients[4])
  expect_that(myperiods, equals(c(4,4,3,4)))
  rfrom(clients[3])
  expect_that(myperiods, equals(rep(4,4)))
})

Sys.sleep(1)
test_that("Experiment replay works", {
  init_data <- function () foo <<- 0
  expt <- experiment(N=1, server="RookServer", autostart=TRUE, on_ready=init_data)
  s1 <- stage(handler=function(id, period, params) {foo <<- foo + 1})
  add_stage(expt, s1)
  t1 <- Sys.time() # time of expt start
  ready(expt)
  snm <- expt$session_name
  
  t2 <- as.numeric(Sys.time() - t1) # time first request at least this  
  expt$handle_request("jim", list(mypar="a"))
  expect_that(foo, equals(1))
  t3 <- as.numeric(Sys.time()-t1) # min time request 2
  expt$handle_request("jim", list(mypar="b"))
  expect_that(foo, equals(2))
  t4 <- as.numeric(Sys.time()-t1) # min time req 3
  expt$handle_request("jim", list(mypar="c"))
  expect_that(foo, equals(3))
  
  Sys.sleep(1)
  replay(expt, maxtime=t4)
  expect_that(foo, equals(2))
  Sys.sleep(1)
  replay(expt, maxtime=t3, folder=snm)
  expect_that(foo, equals(1))
  Sys.sleep(1)
  replay(expt, folder=snm) 
  expect_that(foo, equals(3))
})