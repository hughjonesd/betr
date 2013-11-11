\name{text_stage}
\alias{text_stage}
\title{Create a stage which simply shows some HTML to the user}
\usage{
  text_stage(...)
}
\arguments{
  \item{file}{A filepath. If the file ends in ".brew" then
  it is passed to \code{brew} for processing. Otherwise it
  is shown to the subject as-is.}

  \item{text}{A character vector of HTML. Only one of
  \code{file} and \code{text} should be passed.}
}
\value{
  an object of class TextStage.
}
\description{
  A text stage presents some HTML once to each subject
}

