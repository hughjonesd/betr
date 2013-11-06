
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