#' @import brew
#' @import knitr

# what do do about 'error'?
.onLoad <- function(libname, pkgname) {
  library(knitr)
  library(brew)
  brewCacheOn() # just does parsing, hopefully safe
  opts_chunk$set(echo=FALSE, cache=FALSE, warning=FALSE, message=FALSE)
  opts_knit$set(out.format="html", progress=FALSE, upload.fun = image_uri)
}

call_page <- function(text_or_fn, id, period, params) {
  if (is.character(text_or_fn)) return(text_or_fn)
  if (is.function(text_or_fn)) return(text_or_fn(id, period, params))
  stop("text_or_fn should be character or function, was ", class(text_or_fn))
}
#' Use \code{\link{brew}} within a stage.
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

#' Use \code{\link{knitr}} within a stage.
#' 
#' This returns a function which can be passed as the \code{page} argument to 
#' a Stage object. When the page is shown \code{\link{knit}} will be called
#' on \code{filename}. The variables \code{id}, \code{period} and \code{params}
#' will be available within the knitr file.
#' 
#' @param filename Path of the file to knit
#' 
#' @details
#' By default the following options are set: \code{echo=FALSE, cache=FALSE,
#' warning=FALSE, message=FALSE} for \code{opts_chunk}; and
#' \code{out.format="html", progress=FALSE, upload.fun = image_uri} for
#' \code{opts_knit}. The last option means that images will be shown encoded as
#' "data URIs" in the document itself.
#' 
#' @examples
#' \dontrun{
#' text_stage(page=b_knit("myknitfile.Rhtml"))
#' }
#' @export

b_knit <- function(filename) {
  function(id, period, params) {
    capture.output(knit(filename, output=stdout(), quiet=TRUE))
  }
}
