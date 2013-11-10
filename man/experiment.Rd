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

  \item{client_param}{character. Allows specifying client
  names by HTML parameters, e.g. expt?client=1}
}
\value{
  an object of class Experiment.
}
\description{
  In betr, an experiment consists of one or more stages, as
  well as global options defined when the experiment is
  created.
}
\examples{
expt <- experiment(name='testing', port=12345, N=4)
add_stage(expt, function(...)"<html><body>Hello world!</body><html>")
ready(expt)
}

