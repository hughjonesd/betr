
#' Functions which return functions that can check a form input for errors.
#' 
#' These functions return functions which can be passed to the \code{fields}
#' list of a \code{\link[form_stage]{FormStage}} object. The created functions 
#' check their inputs and return \code{NULL} or an error message.
#' 
#' The examples are mostly self-explanatory.
#' 
#' \code{all_of} checks each of the functions it is passed.
#' 
#' @examples
#' f1 <- is_whole_number()
#' f1("pi", 3.141)
#' is_one_of(1:3)("The variable", 3)
#' is_one_of(1:3)("The variable", 4)
#' is_one_of("a", "b", 2:7)("The variable", 1)
#' length_between(3,5)("The string", "abcdef")
#' length_at_least(5)("The string", "abcd")
#' f1 <- all_of(is_whole_number(), is_between(3,5))
#' f1("The variable", 2)
#' f1("The variable", 2.5)
#' f1("The variable", 3.5)
#' f1("The variable", 4)
#' 
#' @return A function for checking a form input
#' @family checks
#' @export
all_of <- function(...) {
  subchecks <- list(...)
  function(ftitle, val, ...) {
    results <- lapply(subchecks, function(sc) sc(ftitle, val, ...))
    nulls <- sapply(results, is.null)
    if (all(nulls)) return(NULL) 
    results <- results[!nulls]
    if (length(results)==1) return(results[[1]])
    return(c(paste0(ftitle, " has multiple errors:"), unlist(results)))
  }
}

#' @rdname all_of
#' @export
is_at_least <- function(min) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (! val >= min) paste0(sQuote(ftitle), 
      " must be at least ", min) else NULL
  }
}

#' @rdname all_of
#' @export
is_one_of <- function(...) {
  dots <- unlist(list(...))
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    if (! val %in% dots) paste0(sQuote(ftitle), " must be one of: ", 
          paste(dots, collapse=", ")) else NULL
  }   
}

#' @rdname all_of
#' @export
has_value <- function() {
  function(ftitle, val, ...) {
    if (is.null(val) || is.na(val) || nchar(val)==0 ) paste0("Please submit a
      value for ", sQuote(ftitle)) else NULL
  }
  }

#' @rdname all_of
#' @export
is_at_most <- function(max) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (is.null(val) || is.na(val) || ! val <= max) paste0(sQuote(ftitle), 
      " must be no more than ", max) else NULL
  }
}

#' @rdname all_of
#' @export
is_whole_number <- function() {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    tol = .Machine$double.eps^0.5  
    if (is.null(val) || is.na(val) || abs(val - round(val)) >= tol) paste0(sQuote(ftitle), 
      " must be a whole number") else NULL
  }
}

#' @rdname all_of
#' @export
is_between <- function(min, max) {
  function(ftitle, val, ...) {
    if (! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    val <- as.numeric(val)
    if (is.null(val) || is.na(val) || ! (val >= min && val <= max)) paste0(sQuote(ftitle), 
      " must be between ", min, " and ", max) else NULL
  }
}

#' @rdname all_of
#' @export
length_between <- function(min, max) {
  function(ftitle, val, ...) {
    if (min > 0 && ! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    nc <- nchar(val)
    if (! (nc >= min && nc <= max)) paste0(sQuote(ftitle), 
      " must be between ", min, " and ", max, " characters long") else NULL
  }
}


#' @rdname all_of
#' @export
length_at_least <- function(min) {
  function(ftitle, val, ...) {
    nc <- nchar(val)
    if (min > 0 && ! is.null(hv <- has_value()(ftitle, val, ...))) return(hv)
    if (! nc >= min) paste0(sQuote(ftitle), " must be at least ", min, 
      " characters long") else NULL
  }
}