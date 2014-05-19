#' @import brew
#' @import knitr
#' 
#' 

# what do do about 'error'?

brewCacheOn() # just does parsing, hopefully safe
opts_chunk$set(echo=FALSE, cache=FALSE, warning=FALSE, message=FALSE)
opts_knit$set(out.format="html", progress=FALSE, upload.fun = image_uri)
#' Use brew within a stage.
#' 
#' This returns a function which can be passed as the \code{page} argument to 
#' a Stage object. When the page is shown \code{\link{brew}} will be called
#' on \code{filename}. The variables \code{id}, \code{period} and \code{params}
#' will be available within the brew file.
#' 
#' @param filename Path of the file to brew.
#' @examples
#' \dontrun{
#' text_stage(page=b_brew("mybrewfile.html"))
#' }
#' @export
b_brew <- function(filename) {
  function(id, period, params) {
    capture.output(brew(filename))
  }
}

b_knit <- function(filename) {
  function(id, period, params) {
    capture.output(knit(filename, output=stdout()))
  }
}

