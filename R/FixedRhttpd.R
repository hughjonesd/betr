FixedRhttpd <- setRefClass("FixedRhttpd", contains="Rhttpd")
FixedRhttpd$methods(start=function (listen = "127.0.0.1", port = getOption("help.ports"), 
  quiet = FALSE) 
{
  if (nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
    warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
    utils::flush.console()
    return(invisible())
  }
  if (grepl("rstudio", base::.Platform$GUI, ignore.case = TRUE)) {
    warning("Temporarily disabling RStudio's dynamic help")
    try(startDynamicHelp(FALSE), silent=TRUE)
  }
  if (!missing(listen) && listen != "127.0.0.1") {
    listen <- "127.0.0.1"
    warning("This version of Rook can only listen on the loopback device.")
  }
  if (!missing(port)) {
    oldPorts <- getOption("help.ports")
    on.exit(options(help.ports = oldPorts))
    options(help.ports = port)
  }
  if (length(appList) == 0) 
    add(RhttpdApp$new(system.file("exampleApps/RookTestApp.R", 
      package = "Rook"), name = "RookTest"))
  listenPort <<- startDynamicHelp(TRUE)
  if (listenPort == 0) {
    base::stop("The internal web server could not be started!")
  }
  if (!quiet) {
    cat("\nServer started on host", listen, "and port", listenPort, 
      ". App urls are:\n\n")
    invisible(lapply(names(appList), function(i) {
      cat("\thttp://", listen, ":", listenPort, appList[[i]]$path, 
        "\n", sep = "")
    }))
  }
  invisible()
})

