% Generated by roxygen2 (4.0.2): do not edit by hand
\name{write_data}
\alias{write_data}
\title{Write data to "session_name.csv"}
\usage{
write_data(experiment, data_frame)
}
\arguments{
\item{experiment}{an object of class Experiment}

\item{data_frame}{a data frame to write to a file}
}
\description{
This is a simple convenience function that calls \code{\link{write.csv}}
with the file name set to the experimental session name.
}
\seealso{
Other development tools: \code{\link{experiment_data_frame}};
  \code{\link{identify_seats}}; \code{\link{web_test}}
}

