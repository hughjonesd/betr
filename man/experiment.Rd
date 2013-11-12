\name{experiment}
\alias{experiment}
\title{Create an experiment.}
\usage{
  experiment(...)
}
\arguments{
  \item{auth}{may be TRUE, FALSE, a character vector of
  regular expressions, or a function taking two arguments,
  client and params.}

  \item{port}{what port to listen on}

  \item{autostart}{logical. Start the experiment
  automatically when N participants have joined?}

  \item{allow_latecomers}{logical. Allow participants to
  join after the experiment has started?}

  \item{N}{a numeric giving how many participants are
  required}

  \item{server}{a class name (quoted or unquoted) of a
  betr::Server subclass, or an instance object. Typical:
  "RookServer"}

  \item{name}{the character name of the experiment, used in
  creating folders.}

  \item{client_refresh}{numeric. How often should waiting
  clients refresh thier pages?}

  \item{clients_in_url}{logical. If \code{TRUE}, client
  names can be specified in the URL as e.g.
  experiment/client_name. Useful for testing, should be
  turned off in production!}
}
\value{
  an object of class Experiment.
}
\description{
  In betr, an experiment consists of one or more stages, as
  well as global options defined when the experiment is
  created.
}
\details{
  An experiment is typically created in a source file,
  which also adds one or more stages to it using
  \code{\link{add_stage}}. When you run the experiment, you
  source this file. Call
  \code{\link[=ready]{ready(experiment)}} when you want
  subjects to be able to connect to the server. They will
  see a waiting page which refreshes regularly. To see your
  experiment's status, call
  \code{\link[=info]{info(experiment)}} or simply type
  \code{experiment} on the command line. When you want the
  experiment to start, call
  \code{\link[=start]{start(experiment)}}.

  An experiment has its own empty environment. Functions
  and brew files passed into stages will be evaluated in
  this environment. When the experiment is replayed, this
  environment is cleaned. It is a good idea to assign
  variables into the experiment's environment rather than
  elsewhere: \code{ with(environment(expt), mydf <-
  data.frame(id=character(0), profit=character(0))) } This
  will keep your experiments replay-safe.
}
\examples{
expt <- experiment(name='testing', port=12345, N=4)
add_stage(expt, function(...)"<html><body>Hello world!</body><html>")
ready(expt)
}

