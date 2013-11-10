
test_that("Command line server works remotely", {
  cls <- CommandLineServer$new(port=12345, 
    pass_request = function(name, params) return(params["foo"]))
  cls$start()
  tmp <- socketConnection(port=12345)
  cat("test foo:bar\n", file=tmp)
  Sys.sleep(0.2)
  resp <- readLines(tmp)
  expect_that(resp, matches("bar"))
  cls$halt()
  close(tmp)
  expect_that(socketConnection(port=12345), gives_warning())
})

test_that("RookServer works", {
  rs <- RookServer$new(name="jim", session_name="jim-at-midnight",
        clients_in_url=FALSE,
        pass_request = function(name, params) return(params["foo"]))
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
  # TODO add clclient, or is this tested in servers?
})


Sys.sleep(1) # new session name
test_that("Rook server works with experiment", {
  expt <- experiment(N=1, server=RookServer)
  s1 <- stage(handler=function(...) "Got to stage s1")
  add_stage(expt, s1)
})