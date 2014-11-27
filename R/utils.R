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
#' @param browser name of the browser executable. See \code{\link{browseURL}}.
#' @family development tools
#' @export
web_test <- function (experiment, N=ifelse(is.finite(experiment$N), 
      experiment$N, 1), clients_in_url=TRUE, ids=paste("client", 1:N, sep="-"),
      browser=getOption('browser')) {
  if (experiment$status=="Stopped") warning("Experiment status is Stopped. Try calling ready() first")
  for (i in 1:N) {
    browseURL(paste0(get_url(experiment), if(clients_in_url) paste0("/", ids[[i]])),
        browser=browser)
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
#' for when \code{\link{ready}} is called. The file can be created 
#' manually - this function is just for convenience. 
#' The file format is like:
#' \preformatted{
#' seat IP  cookie
#' 1  111.1.1.123 AFDJKLRE
#' 2  111.1.1.124 REAJKJKL
#' ...
#' }
#' Either IP or cookie may be NA. If the (default) IP method is used, seats will
#' be identified by IP address: this requires static IP addresses for your
#' clients. The cookie method sets a cookie named \code{betr-seat} on the client
#' browser, which will only work on a per-browser basis. If method is 'both'
#' then both cookie and IP address will be used; if both cookie and IP address
#' match the seat file, betr will use the cookie.
#' @export
identify_seats <- function (method="IP", serve=TRUE) {
  if (! method %in% c("IP", "cookie", "both")) stop(
        "'method' must be one of 'IP', 'cookie' or 'both'")
  app <- function(env) {
    req <- Rook::Request$new(env)
    res <- Rook::Response$new()
    IP <- req$ip()
    if (length(IP) == 0) {
      IP <- "127.0.0.1" 
      if (exists("HTTP_X_FORWARDED_FOR", env)) {
        IP <- sub("^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}).*", "\\1",
          env$HTTP_X_FORWARDED_FOR)
      }
    }
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
            <input type='submit'>")
      res$write("<table>")
      for (r in 0:3) {
        res$write("<tr>")
        for (c in 1:10) {
          num <- r * 10 + c
          res$write(sprintf("<td><button type='submit' name='seat' value='%s' 
                style='background:blue; color:white; height:75px; width:50px; 
                font-size: 200%%;'>", num))
          res$write(paste(num,"</button>"))
        }
        res$write("</tr>")
      }
      res$write("</table></form></body></html>")
    }
    res$finish()
  }
  if (serve) {
    svr <- Rhttpd$new()
    rhapp <- RhttpdApp$new(name="seats", app=app)
    svr$add(rhapp)
    require(tools)
    svr$start(port=35538)
    cat("Serving on", svr$full_url(1))
  } else {
    return(app)
  }
}



#' Create an appropriate data frame for an experiment
#' 
#' This is a convenience function. It creates a data frame with \code{N * periods}
#' rows, with a column \code{id} varying from 1 to \code{N} and a column \code{period} varying
#' from 1 to \code{periods}. The data frame is sorted by period, then by ID. 
#' \code{N} and \code{periods} can be specified manually, or you can just pass in 
#' the \code{\link{experiment}} object and the function will guess for you.
#' 
#' @param N number of subjects
#' @param periods number of periods
#' @param experiment an Experiment object
#' @param ... other arguments to data.frame
#' 
#' @family development tools
#' @examples
#' 
#' expt <- experiment(N=5)
#' s1 <- text_stage(page="<html><body>got here</body></html>")
#' add_stage(expt, s1, period(), times=5)
#' mydf <- experiment_data_frame(expt)
#' 
#' @export
experiment_data_frame <- function(experiment=NULL, N=NULL, periods=NULL, ...) {
  if (! is.null(experiment)) {
    N <- experiment$N
    periods <- nperiods(experiment)
  }
  if (periods < 1) stop("Experiment must have at least one period!")
  arglist <- list(...)
  arglist$period <- rep(1:periods, each=N)
  arglist$id <- rep(1:N, periods)
  arglist <- arglist[c("id", "period", setdiff(names(arglist), c("id", "period")))]
  do.call(data.frame, arglist)
}

#' Write data to "session_name.csv"
#' 
#' This is a simple convenience function that calls \code{\link{write.csv}}
#' with the file name set to the experimental session name.
#' 
#' @param experiment an object of class Experiment
#' @param data_frame a data frame to write to a file
#' 
#' @family development tools
#' 
#' @export
write_data <- function(experiment, data_frame) {
  fn <- session_name(experiment)
  if (is.na(fn)) {
    fn <- paste0("betr-data-", paste0(sample(LETTERS, 10), collapse=""))
  }
  message("Writing data frame to ", fn, ".csv")
  write.csv(data_frame, file=paste0(fn, ".csv"))
}


#' Set up simple one-word commands
#' 
#' After \code{load_commands(expt)} is called, entering \code{READY}, 
#' \code{START}, \code{HALT}, \code{PAUSE}, \code{RESTART}, \code{INFO},
#' \code{MAP}, \code{NEXT_STAGE} or \code{WEB_TEST} will call the corresponding 
#' command on the experiment, with no arguments.
#' 
#' @param expt An object of class \code{Experiment}
#' @details
#' \code{load_commands} is implemented using \code{\link{makeActiveBinding}}.
#' For obvious reasons, don't assign to the values \code{READY} etc. either
#'  before or after this is called! 
#' 
#' \code{unload_commands} simply removes the command names from the global 
#' environment.
#'  
#' @family command line functions
#' @examples
#' expt <- experiment(name="myexpt", N=2, seats_file=NULL, record=FALSE)
#' info(expt)
#' load_commands(expt)
#' INFO
#' @export
load_commands <- function(expt) {
  en <- parent.frame()
  makeActiveBinding("READY", function(x) ready(expt), env=en)
  makeActiveBinding("START", function(x) start(expt), env=en)
  makeActiveBinding("HALT", function(x) halt(expt), env=en)
  makeActiveBinding("PAUSE", function(x) pause(expt), env=en)
  makeActiveBinding("RESTART", function(x) restart(expt), env=en)
  makeActiveBinding("INFO", function(x) info(expt), env=en)
  makeActiveBinding("MAP", function(x) map(expt), env=en)
  makeActiveBinding("WEB_TEST", function(x) web_test(expt), env=en)
  makeActiveBinding("NEXT_STAGE", function(x) next_stage(expt), env=en)
}

unload_commands <- function() {
  rm(READY, START, HALT, PAUSE, RESTART, INFO, MAP, WEB_TEST, NEXT_STAGE,
        envir=parent.frame())
}

#' Simple HTML header and footer
#' 
#' Convenience functions to print some simple HTML
#'  
#' @family HTML utilities
#' 
#' @param title Page title.
#' @param refresh Number of seconds after which the page should refresh from the
#'        server.
#' @param text Text of the submit button.
#' @details
#' \code{header} prints an HTML header with an optional title and refresh time.
#' 
#' \code{footer} prints the corresponding footer.
#' 
#' \code{next_form} prints a simple HTML form with a single submit button. NB: do
#' not use \code{next_form} within your own <form> tags. That will cause malformed
#' HTML and break your web page. Instead just do it yourself:
#' "<input type='submit' value='Next'>"
#' 
#' @examples
#' s1 <- text_stage(page=paste0(header(), "<b>Got here!</b>", next_form(), 
#'      footer()))
#' @export
header <- function(title="Experiment", refresh=NA) {
  paste0("<html><head><title>", title, "</title>", 
        if(!is.na(refresh)) sprintf("<meta http-equiv='refresh' content='%d'>", 
        refresh) ,"</head>
        <body style='background-color: #CCCCCC; padding: 2% 4%;'>
        <div style='background-color: white; padding: 3% 3%; 
        border: 1px solid #888888; border-radius: 10px;'>")
}

#' @rdname header
#' @export
footer <- function() {
  "</div><div align='center' style='padding: 10px 10px;'>betr</div></body></html>"
}

#' @rdname header
#' @export
next_form <- function(text='Next') sprintf("<form action='' method='POST'>
      <input type='submit' value='%s' /></form>", text)
