#' @include betr.R
NULL

#' Open multiple browser windows to test an experiment
#' 
#' NB: by default this opens one window for each participant up to a total of 
#' \code{N}!
#' @param experiment an object of class Experiment
#' @param N how many windows to open
#' @param clients_in_url if \code{TRUE}, each URL will be given a unique 
#'        client identifier. This only works if \code{experiment} has
#'        \code{clients_in_url} set to \code{TRUE}
#' @param ids character vector of ids to supply in the URLs opened
#' @family development tools
#' @export
browser_test <- function (experiment, N=ifelse(is.finite(experiment$N), 
      experiment$N, 1), clients_in_url=TRUE, ids=paste("client", 1:N, sep="-")) {
  for (i in 1:N) {
    browseURL(paste0(get_url(experiment), if(clients_in_url) paste0("/", ids[[i]])))
  }
}
