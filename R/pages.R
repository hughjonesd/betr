#' @import brew
#' @import knitr

# what do do about 'error'?
.onLoad <- function(libname, pkgname) {
  library(knitr)
  library(brew)
  brewCacheOn() # just does parsing, hopefully safe
  opts_chunk$set(echo=FALSE, cache=FALSE, warning=FALSE, message=FALSE,
        results='asis')
  opts_knit$set(out.format="html", progress=FALSE, upload.fun = image_uri)
}

call_page <- function(text_or_fn, id, period, params, errors=character(0)) {
  if (is.character(text_or_fn)) return(paste(text_or_fn, collapse="\n"))
  if (is.function(text_or_fn)) {
    res <- text_or_fn(id, period, params, errors)
    if (is.next(res) || is.wait(res)) return(res)
    return(paste(res, collapse="\n"))
  }
  stop("text_or_fn should be character or function, was ", class(text_or_fn))
}
#' Use \code{\link{brew}} within a stage.
#' 
#' This returns a function which can be passed as the \code{page} argument to 
#' a Stage object. When the page is shown \code{\link{brew}} will be called
#' on \code{filename}. The variables \code{id}, \code{period}, \code{params}
#' and \code{error} will be available within the brew file. \code{params} is
#' a named list of HTML parameters. \code{error} is a character vector of errors,
#' e.g. from a form submission. Typically it will be \code{character(0)}.
#' 
#' 
#' @param filename Path of the file to brew.
#' @examples
#' \dontrun{
#' text_stage(page=b_brew("mybrewfile.html"))
#' }
#' @family page creation
#' @export
b_brew <- function(filename) {
  function(id, period, params, errors) {
    capture.output(brew(filename))
  }
}

#' Use \code{\link{knitr}} within a stage.
#' 
#' This returns a function which can be passed as the \code{page} argument to 
#' a Stage object. When the page is shown \code{\link{knit}} will be called
#' on \code{filename}. The variables \code{id}, \code{period}, \code{params}
#' and \code{error} will be available within the knitr file. \code{params} is
#' a named list of HTML parameters. \code{error} is a character vector of errors,
#' e.g. from a form submission. Typically it will be \code{character(0)}.
#' 
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
#' @family page creation
#' @export
b_knit <- function(filename) {
  function(id, period, params, errors) {
    capture.output(invisible(knit(filename, output=stdout(), quiet=TRUE)))
  }
}


