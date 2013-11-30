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

#' Starts a web server to allow you to identify computer seat numbers
#' 
#' After calling this function, go to each computer, start the web 
#' browser you use for experiments, and browse to <server IP>:<server port>
#' /custom/seats . Then you can enter the computer's seat number. 
#' 
#' @param method one of 'IP', 'cookie' or 'both'. See below.
#' @param serve if TRUE, starts the \code{\link{Rhttpd}} web server
#'        to serve the application. If FALSE the function returns
#'        a Rook app which can be served by your preferred method
#' @family development tools
#' @details
#' Seat details are stored in a file betr-SEATS.txt, which is looked
#' for when \code{\link{ready()}} is called. The file can be created 
#' manually - this function is just for convenience. 
#' The file format is like:
#' \preformatted{
#' seat IP  cookie
#' 1  111.1.1.123 AFDJKLRE
#' 2  111.1.1.124 REAJKJKL
#' ...
#' }
#' Either IP or cookie may be NA. If the (default) IP method is used,
#' seats will be identified by IP address: this requires static
#' IP addresses for your clients. The cookie method sets a cookie
#' on the client browser, which will only work on a per-browser basis.
#' If method is 'both' then both cookie and IP address will be used;
#' if both cookie and IP address match the seat file, betr will use
#' the cookie.
#' @export
identify_seats <- function (method="IP", serve=TRUE) {
  if (! method %in% c("IP", "cookie", "both")) stop(
        "'method' must be one of 'IP', 'cookie' or 'both'")
  app <- function(env) {
    req <- Rook::Request$new(env)
    res <- Rook::Response$new()
    IP <- req$ip()
    p <- req$params()
    ck <- req$cookies()
    error <- ""
    if ("seat" %in% names(p)) tryCatch({ 
      if (is.null(IP) && method != "cookie") {
        stop("Couldn't find IP address for seat", p$seat)
      }
      if (method=="cookie") IP <- NA
      if (! "betr-seat" %in% names(ck) && method !="IP") {
        stop("Couldn't find cookie for seat", p$seat)
      } else {
        cookie <- ck$"betr-seat"
      }
      # write seat, IP, cookie to file
      if (! file.exists("betr-SEATS.txt")) {
        sf <- file("betr-SEATS.txt", "a")        
        cat("seat\tIP\tcookie\n", file=sf)
      } else {
        seats <- read.table("betr-SEATS.txt")
        if (p$seat %in% seats$seat) stop("Seat already defined in betr-SEATS.txt
              (did you enter the same seat number twice?)")
        sf <- file("betr-SEATS.txt", "a")        
      }
      cat(sprintf("%s\t%s\t%s\n", p$seat, IP, cookie), file=sf)
      res$write(sprintf("<html><body><h1>Successfully assigned seat %s in betr-SEATS.txt</h1>", p$seat))
      if (! is.na(IP)) res$write(paste("<p>IP address:", IP, "</p>"))
      if (! is.na(cookie)) res$write(paste("<p>betr-seat cookie ID:", cookie, "</p>"))
      res$write("</body></html>")
    }, error = function(e) error <<- e$message)
    
    if (nchar(error) > 0 || ! "seat" %in% names(p)) {
      res$set_cookie("betr-seat", paste(sample(LETTERS, 10, replace=T), 
            collapse=""))
      res$write("<html><body>")
      if (nchar(error)>0) res$write(sprintf("<p style='color:red'>%s</p>", 
            error))
      res$write("<h1>Enter seat number</h1>
            <form action='' method=POST>Enter this computer's seat number:
            <br><input type='text' name='seat' width='4'>
            <input type='submit'></form></body></html>")
    }
    res$finish()
  }
  if (serve) {
    svr <- Rhttpd$new()
    rhapp <- RhttpdApp$new(name="seats", app=app)
    svr$add(rhapp)
    svr$start()
    cat("Serving on", svr$full_url(1))
  } else {
    return(app)
  }
}
