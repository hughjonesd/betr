
library(testthat)
library(betr)

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
  expt <- experiment(N=2, server="CommandLineServer", record=FALSE, seats_file=NULL)
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


test_that("Experiment starting, status, pause, restart & halt work", {
  expt <- experiment(N=2, server="CommandLineServer", record=FALSE, seats_file=NULL)
  add_stage(expt, period(), text_stage(page="foo"))
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
  
  expect_that(expt$handle_request(client="B", params=list()), 
    equals("foo"))
  pause(expt)
  expect_that(info(expt), prints_text("Paused"))
  expect_that(expt$handle_request(client="B", params=list()), 
    equals(expt$waiting_page("Experiment paused")))
  restart(expt)
  expect_that(info(expt), prints_text("Started"))
  
  expect_that(expt$handle_request(client="A", params=list()), 
    equals("foo"))
  expect_that(expt$handle_request(client="B", params=list()), 
    matches("finished"))
  expect_that(halt(expt), gives_warning())
  expect_that(expt$handle_request(client="A", params=list()), 
    matches("finished"))
  
  halt(expt)
  expect_that(show(expt), prints_text("Stopped"))
  expect_that(expt$handle_request(client="A", params=list()), 
    throws_error())
})

test_that("Command line server & client work", {
  expt <- experiment(N=1, server=CommandLineServer, record=FALSE, seats_file=NULL)
  s1 <- stage(handler=function(...) "Got to stage s1")
  add_stage(expt, s1)
  ready(expt)
  expt$handle_request("jim", list())
  start(expt)
  expt$handle_request("jim", list())
  # TODO add clclient, or is this tested in servers?
})


test_that("Rook server works with experiment", {
  expt <- experiment(N=1, server="RookServer", autostart=TRUE, record=FALSE,
        seats_file=NULL)
  s1 <- stage(handler=function(...) "Got to stage s1")
  add_stage(expt, s1)
  ready(expt)
  expect_that(show(expt), prints_text("Waiting"))
  expect_that(expt$handle_request("jim", list()), equals("Got to stage s1"))
  
  expt <- experiment(N=1, server="RookServer", autostart=FALSE, record=FALSE,
        seats_file=NULL)
  add_stage(expt, s1)
  ready(expt)
  expect_that(expt$handle_request("jim", list()), 
        equals(expt$waiting_page("Waiting to start")))
  start(expt)
  expect_that(expt$handle_request("jim", list()), equals("Got to stage s1"))
  expt$handle_request("jim", list())
})

test_that("TextStages work", {
  s1 <- text_stage(page="Foo")
  expect_that(s1$handle_request(1,1, list()), equals("Foo"))
  expect_that(s1$handle_request(2,1, list()), equals("Foo"))
  expect_that(s1$handle_request(2,1, list()), equals(NEXT))
})

test_that("Stage naming works", {
  s1 <- text_stage(page="Foo", name="my text stage")
  s2 <- period(name="my period")
  s3 <- checkpoint(name="my checkpoint")
  s4 <- stage(name="my stage", handler=function(...) NEXT)
  s5 <- program("all", function(...) NEXT, name="my program")
  s6 <- timed(stage(name="my timed stage", handler=function(...) NEXT), 
        timeout=10)
  ex <- experiment(N=1)
  add_stage(ex, s1, s2, s3, s4, s5, s6)
  tmp <- capture.output(print_stages(ex))
  expect_that(tmp[1], matches("my text stage"))
  expect_that(tmp[2], matches("my period"))
  expect_that(tmp[3], matches("my checkpoint"))
  expect_that(tmp[4], matches("my stage"))
  expect_that(tmp[5], matches("my program"))
  expect_that(tmp[6], matches("my timed stage"))
})

test_that("brew pages work", {
  t2 <- tempfile("text_stage_test", fileext=".brew")
  cat("bar:<%= bar %> id:<%= id %> period:<%= period %>\n", file=t2)
  bar <<- "boing"
  s3 <- text_stage(page=b_brew(t2))
  expect_that(s3$handle_request(1,1, list()), equals("bar:boing id:1 period:1"))  
})

test_that("Checkpoints work", {  
  expt <- experiment(N=4, server="RookServer", autostart=FALSE, 
    randomize_ids=FALSE, record=FALSE, seats_file=NULL)
  s1 <- text_stage("foo")
  s2 <- text_stage("bar")
  add_stage(expt, period(), s1, checkpoint(), s2, checkpoint(c(1,2,1,2)),
        s1)
  ready(expt)
  
  rfrom <- function(cl) expt$handle_request(cl, list())
  clients <- paste0("client-", 1:4)
  sapply(clients, rfrom)
  start(expt)
  sapply(clients, rfrom)
  expect_that(rfrom(clients[1]), matches("Waiting"))
  sapply(clients[2:3], rfrom)
  expect_that(rfrom(clients[4]), equals("bar"))
  expect_that(sapply(clients[1:3], rfrom), matches("bar"))
  expect_that(sapply(clients[1:2], rfrom), matches("Waiting"))
  expect_that(rfrom(clients[3]), equals("foo"))
  expect_that(rfrom(clients[4]), equals("foo"))
  expect_that(sapply(clients[1:2], rfrom), matches("foo"))  
})

test_that("Programs work", {
  first <- NA
  seen <- numeric(0)
  last <- NA
  expt <- experiment(N=3, server="RookServer", autostart=FALSE, 
    randomize_ids=FALSE, record=FALSE, seats_file=NULL)
  s1 <- program("first", function (id, period) first <<- id)
  s2 <- program("all", function(id, period) seen <<- c(seen, id))
  s3 <- program("last", function(id, period) last <<- id)
  add_stage(expt, period(), s1, s2, s3)
  ready(expt)
    
  rfrom <- function(cl) expt$handle_request(cl, list())
  clients <- paste0("client-", 1:3)
  sapply(clients, rfrom)
  start(expt)
  rfrom(clients[1])
  expect_that(first, equals(1))
  sapply(clients[2:3], rfrom)
  expect_that(last, equals(3))
  expect_that(seen, equals(1:3))
})
  
test_that("Periods work", {
  init_data <- function() {
    myperiods <<- rep(NA,4) 
    seen <<- matrix(FALSE, nrow=4, ncol=4)
  }
  expt <- experiment(N=4, server="RookServer", autostart=FALSE, 
        randomize_ids=FALSE, on_ready=init_data, record=FALSE, seats_file=NULL)
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

test_that("Timed periods work", {
  init_data <- function () {
    mydf <<- experiment_data_frame(expt)
    mydf$timed_out <<- FALSE
  }
  expt <- experiment(N=2, server="RookServer", autostart=FALSE, 
        on_ready=init_data, randomize_ids=FALSE, record=FALSE, seats_file=NULL)
  s1 <- function(id, period, params) "s1"
  s1t <- timed(s1, timeout=5)
  s1tf <- timed(s1, timeout=5, on_timeout=function(id, period) 
        mydf$timed_out[mydf$id==id & mydf$period==period] <<- TRUE)
  add_stage(expt, period(), s1t, text_stage("foo"), s1tf)
  
  rfrom <- function(cl) expt$handle_request(cl, list())
  clients <- paste0("client", 1:2)
  ready(expt)
  rfrom(clients[1])
  rfrom(clients[2])
  start(expt)
  rfrom(clients[1])
  expect_that(rfrom(clients[1])$body, equals("s1"))
  rfrom(clients[2])
  Sys.sleep(6)
  expect_that(rfrom(clients[2]), equals("foo"))
  
  next_stage(expt, 1)
  rfrom(clients[1]) # foo
  rfrom(clients[1])
  rfrom(clients[1])
  expect_that(mydf$timed_out[mydf$id==1 & mydf$period==1], is_false())
  rfrom(clients[2])
  Sys.sleep(6)
  rfrom(clients[2])
  expect_that(mydf$timed_out[mydf$id==2 & mydf$period==1], is_true())
})

test_that("Experiment replay works", {
  td <- tempdir()
  od <- setwd(td)
 
  init_data <- function () {foo <<- 0; bar <<- 0}
  expt <- experiment(N=1, autostart=TRUE, on_ready=init_data, seats_file=NULL)
  s1 <- stage(handler=function(id, period, params) {foo <<- foo + 1; 
        bar <<- sample(1:10000000, 1)})
  add_stage(expt, s1)
  ready(expt)
  t1 <- Sys.time() # time of expt start + a bit
  snm <- expt$session_name
  
  t2 <- as.numeric(Sys.time() - t1) # time first request at least this  
  expt$handle_request("jim", list(mypar="a"))
  expect_that(foo, equals(1))
  t3 <- as.numeric(Sys.time()-t1) # min time request 2
  expt$handle_request("jim", list(mypar="b"))
  expect_that(foo, equals(2))
  t4 <- as.numeric(Sys.time()-t1) # min time req 3
  oldbar <- bar
  expt$handle_request("jim", list(mypar="c"))
  expect_that(foo, equals(3))
  Sys.sleep(1)
  replay(expt, maxtime=t4)
  expect_that(foo, equals(2))
  expect_that(bar, equals(oldbar), "random number generation was different")
  Sys.sleep(1)
  replay(expt, maxtime=t3, folder=snm)
  expect_that(foo, equals(1))
  Sys.sleep(1)
  replay(expt, folder=snm) 
  expect_that(foo, equals(3))
  halt(expt, force=TRUE)
  unlink(list.files(pattern=paste0("^", session_name(expt), "$")), recursive=TRUE)
  
  expt <- experiment(N=1, seats_file=NULL)
  tm <<- numeric(0)
  add_stage(expt, stage(function(...) tm <<- c(tm, expt$elapsed_time()) ))
  ready(expt)
  expt$handle_request("client1", list())
  start(expt)
  Sys.sleep(1)
  expt$handle_request("client1", list())
  halt(expt, force=TRUE)
  replay(expt)
  expect_that(round(tm[1],2), equals(round(tm[2],2)), 
        info="Replay didn't get time right")
  setwd(od)
})