% Generated by roxygen2 (4.0.0): do not edit by hand
\name{all_of}
\alias{all_of}
\alias{is_at_least}
\alias{is_at_most}
\alias{is_between}
\alias{is_whole_number}
\alias{length_at_least}
\alias{length_between}
\title{Functions which return functions that can check a form input for errors.}
\usage{
all_of(...)

is_at_least(min)

is_at_most(max)

is_whole_number()

is_between(min, max)

length_between(min, max)

length_at_least(min)
}
\value{
A function for checking a form input
}
\description{
These functions return functions which can be passed to the \code{fields}
list of a \code{\link[form_stage]{FormStage}} object. The created functions
check their inputs and return \code{NULL} or an error message.
}
\details{
\code{all_of} checks each of the functions it is passed.
}
\examples{
f1 <- is_whole_number()
f1("pi", 3.141)
}
