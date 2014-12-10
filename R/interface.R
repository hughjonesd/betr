#' @include betr.R

setGeneric("print") # do I need this?

#' @rdname info
#' @aliases print,Experiment,ANY-method
#' @aliases show,Experiment,ANY-method
#' @usage experiment
setMethod("print", "Experiment", function(x, ...) x$info(FALSE, FALSE))
setMethod("show", "Experiment", function(object) object$info(FALSE, FALSE))

#' Create an experiment.
#' 
#' In betr, an experiment consists of one or more stages, as well
#' as global options defined when the experiment is created.
#' 
#' @param auth TRUE, FALSE, a character vector of patterns,
#'        or a function. See Details
#' @param port what port to listen on
#' @param autostart logical. Start the experiment automatically when N 
#'        participants have joined?
#' @param allow_latecomers logical. Allow participants to join after the
#'        experiment has started?
#' @param N a numeric giving how many participants are required
#' @param server a class name (quoted or unquoted) of a betr::Server 
#'        subclass, or an instance object. Typical: "RookServer"
#' @param name the character name of the experiment, used in creating
#'        folders.
#' @param client_refresh numeric. How often should waiting clients refresh
#'        their pages?
#' @param clients_in_url logical. If \code{TRUE}, client names can be specified 
#'        in the URL as e.g. experiment/client_name. Useful for testing, should
#'        be turned off in production!
#' @param seats_file path of the file where seat information is stored. See
#'        \code{\link{identify_seats}} for details. Note: to suppress warnings
#'        about a missing file, use \code{seats_file=NULL}.
#' @param on_ready a user-defined function, to be called when \code{\link{ready}} 
#'        is called.
#' @param randomize_ids if \code{TRUE}, subject IDs will be randomized from
#'        1 to \code{N}. If \code{FALSE} subject IDs will be allocated first-come
#'        first-served.
#' @param record records experiment commands to disk. Turning this off will save
#'        disk space and not clutter your working directory, but will prevent
#'        experiment replay.
#' @param seed a seed to set whenever \code{\link{ready}} is called. You should
#'        ensure you set this to a different value in every session. 
#'        
#' @return an object of class Experiment.
#' 
#' @details 
#' An experiment is typically created in a source file, which also adds one or 
#' more stages to it using \code{\link{add_stage}}. When you run the experiment,
#' you source this file. Call \code{\link[=ready]{ready(experiment)}} when you
#' want subjects to be able to connect to the server. They will see a waiting
#' page which refreshes regularly. To see your experiment's status, call
#' \code{\link[=info]{info(experiment)}} or simply type \code{experiment} on the
#' command line. When you want the experiment to start, call 
#' \code{\link[=start]{start(experiment)}}.
#' 
#' To keep your experiments replay-safe, use \code{on_ready} to 
#' initialize your data.
#' 
#' The parameter \code{auth} determines how you authorize clients. \code{TRUE} 
#' (the default) allows any client to join the experiment. If \code{auth} is a
#' character vector, it is treated as a list of patterns in shell-glob style, e.g.
#' \code{"192.168.*.*"} (see \code{\link{glob2rx}} for details). If the
#' client's IP address matches any pattern, the client will be
#' accepted. IF \code{auth} is a function, it will be called like \code{auth(ip,
#' params, cookies)} where \code{ip} is the remote IP address and \code{params}
#' and \code{cookies} are lists of HTTP parameters and cookies respectively. The
#' client will be authorized if the function returns \code{TRUE}.
#' 
#' @examples
#' expt <- experiment(name='testing', port=12345, N=4)
#' add_stage(expt, function(...)"<html><body>Hello world!</body><html>")
#' ready(expt)
#' 
#' @export
experiment <- function (...) Experiment$new(...)

#' Add one or more stages to an experiment
#' 
#' @param experiment an Experiment object 
#' @param ... one or more Stage objects, or functions
#' @param times how many times to repeat the sequence of stages in \code{...}.
#'        All stages are repeated if this is a single number; if it is a
#'        vector it gives how many times to repeat each stage.
#' @param each how many times to repeat each individual stage
#' @param after Add stages after how many stages (default: at the end)
#' @usage add_stage(experiment, ..., times, each, after)
#' @details
#' If functions are passed in to \code{add_stage}, Stage objects will 
#' automatically be created from them. 
#' Stage objects are \link[=ReferenceClasses]{reference classes}. However,
#' when added to the experiment, they are copied. So, changing the 
#' Stage after adding it to the experiment will not work.
#' @examples
#' expt <- experiment(N=1, autostart=TRUE)
#' s1 <- stage(function(id, period, params) return("Got to s1!"))
#' # Or just define the function directly:
#' s2 <- function(id, period, params) return("Got to s2!") 
#' add_stage(expt, s1, s2, times=2) # s1 s2 s1 s2
#' add_stage(expt, s1, s2, times=1:2) # s1 s2 s2
#' add_stage(expt, s1, s2, each=2) # s1 s1 s2 s2
#' info(expt)
#' @export
add_stage <- function (experiment, ...) 
  experiment$add_stage(...)

setGeneric("start") 
#' Start the experiment running.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{start} moves the experiment from Waiting to Started.
#' If the experiment has autostart set, this will happen automatically
#' when N subjects have connected. 
#' When the experiment starts, all subjects are moved to the first stage.
#' @param experiment Object of class Experiment
#' @param force Start the experiment even if there are fewer than N participants
#' @details 
#' Note that \code{start} is an S3 generic to avoid clashes with \code{start} in
#' the stats package.
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @method start Experiment
#' @examples
#' \dontrun{
#' start(expt)
#' }
#' @export
start.Experiment <- function(experiment, force=FALSE) experiment$handle_command("start",
  list(force=force))


#' Set the experiment up to receive participants.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{ready(experiment)} moves the experiment from Stopped to Waiting.
#' Clients can now connect and will be shown a waiting page.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
ready <- function(experiment) experiment$ready()


#' Pause the experiment.
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{pause(experiment)} moves the experiment from Started to Paused.
#' Clients will be shown a waiting page until the experiment is continued.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
pause <- function(experiment) experiment$handle_command("pause")

#' Restart the experiment after pausing
#' 
#' Experiments can be in status Stopped, Waiting, Started, or Paused.
#' \code{restart(experiment)} moves the experiment from Paused back to Started.
#' 
#' @param experiment Object of class Experiment
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
restart <- function(experiment) experiment$handle_command("restart")

#' Move some clients forward one stage
#' 
#' Manually moves one or more clients forward. This may break your experimental
#' design so use in emergencies only!
#' 
#' @param experiment Object of class Experiment
#' @param subjid numeric vector of subject id(s) to move forward, or data 
#'        frame from subjects table
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
next_stage <- function(experiment, subjid=1:experiment$N) {
  warning("Moving subjects on manually")
  experiment$handle_command("next_stage", list(subj=subjid))
}


#' Halt the experiment, stopping the server
#' 
#' @param experiment Object of class Experiment
#' @param force If TRUE, force the experiment to halt even if participants are
#'        not finished.
#' @return TRUE or FALSE, invisibly
#' @family command line functions
#' @export
halt <- function(experiment, force=FALSE) experiment$halt(force)

#' Show basic info about an experiment
#' 
#' \code{info} prints information about the status, including session name,
#' number of stages, number of clients connected and total N, 
#' status (Stopped, Waiting, Started or Paused)
#' and the URL where the experiment is serving.
#' \code{map} shows a map of how subjects are progressing through the stages.
#' \code{get_url} returns the experiment url.
#' \code{nperiods} returns the number of \code{\link{period}}s in the experiment.
#' \code{session_name} returns the experiment session name, 
#' or NA if the experiment status is Stopped.
#' \code{print_stages} prints a list of the stages in the experiment.
#' @param experiment an object of class Experiment
#' @param subj if TRUE, print the subjects table
#' @param map if TRUE, also calls \code{map}
#' 
#' @examples
#' expt <- experiment(N=2, port=12345)
#' expt # on the command line, calls info() 
#' 
#' @family command line functions
#' @export
info <- function(experiment, subj=TRUE, map=TRUE) experiment$info(subj, map)

#' @rdname info
#' @export
map <- function(experiment) experiment$map()

#' @rdname info
#' @export
get_url <- function(experiment) experiment$get_url()

#' @rdname info
#' @export
session_name <- function(experiment) experiment$get_session_name()


#' @rdname info
#' @export
nperiods <- function(experiment) experiment$nperiods()

#' @rdname info
#' @export
print_stages <- function(experiment) experiment$print_stages()

#' Merge a data frame with information about experiment subjects
#' @param experiment an object of class Experiment
#' @param data_frame a data frame containing a column 'id'
#' 
#' @return A new data frame produced by \code{\link{merge}} using the 'id' 
#' column, resulting in new columns 'IP', 'client' and 'seat'. 'period', 'stage',
#' 'hits' and 'status' will not be merged.
#' 
#' @examples
#' expt <- experiment(N=2, port=12345)
#' expt # on the command line, calls info() 
#' 
#' @family command line functions
#' @export
#' @export
merge_subjects <- function(experiment, data_frame) {
  experiment$merge_subjects(data_frame)
}

#' Replay part or all of an experiment
#' 
#' \code{replay} plays back an experiment using records stored on disk.
#' All requests from clients, and all commands issued on the command line,
#' will be replayed. Afterwards the experiment can be continued.
#' \code{rewind} is a short cut for \code{replay(..., folder=NULL, rewind=TRUE)}
#' 
#' @param experiment an object of class Experiment
#' @param folder which record to use. Default is the current session 
#' or the most recent session found
#' @param speed how long to wait between each command or request
#' @param maxtime replay only 'maxtime' seconds of the original session
#' @param ask ask before replaying each command or request
#' @param live run the replay "live", with the web server continuing to serve
#' @param clients (character vector) only replay requests from \code{clients}
#' @param rewind Set to \code{TRUE} if you are rerunning a live session.
#' @param ... Arguments passed to \code{replay}
#' 
#' @details
#' betr records requests and commands in a folder named <experiment
#' name>-YYYY-MM-DD-HHMMSS, where the date and time record when
#' \code{\link{ready}} was called. Within this folder, the subfolder 'record'
#' holds details. If \code{folder} is not given, the default is either the
#' current session name, or the most recently accessed folder matching the 
#' format above. 
#' 
#' Each file in the folder records a single command or request. The filename
#' records the time after experiment start that the command/request was processed.
#' Details are stored in the file using the YAML format, which is quite 
#' self-explanatory and easy to edit.
#' 
#' Unless \code{live=TRUE}, the experiment will be stopped before replay(if it is already started). In any case, the
#' subject table will be reinitialized and \code{\link{ready}} will be called. 
#' This has the effect of calling any user-defined \code{on_ready} function.
#' 
#' \code{speed} can be numeric or "realtime". A numeric gives the number of
#' seconds to wait before executing each command or request. "realtime" means,
#' go at the speed of the original session. \code{live=TRUE} implies
#' \code{speed="realtime"} unless \code{speed} is explicitly set.
#' 
#' If \code{maxtime} is given, only the first \code{maxtime} seconds of the experiment will be replayed.
#' This is useful if you want to "rewind" the experiment because of lab problems,
#' or during debugging.
#' 
#' If \code{ask} is \code{TRUE} then the experimenter will be prompted before each
#' command or request is replayed:
#' 
#' \itemize{
#'   \item enter or 'n' replays the command/request
#'   \item 'c' continues replaying to the end, without prompting again
#'   \item 's' skips the command/request
#'   \item 'q' skips this and all other commands/requests
#'   \item 'd' shows details of the command/request
#'   \item '?' or 'h' shows a help message
#'   \item Any other item will be evaluated in the global environment
#' } 
#' 
#' If \code{ask} is a number, then the prompt will only be displayed after \code{ask}
#' seconds of the original experiment have been replayed.
#' Note that replay creates a new session. This means that you cannot do
#' 
#' \code{
#' replay(expt, maxtime=30)
#' replay(expt, maxtime=120)
#' }
#' 
#' : the first replay will have created a new session with only the commands 
#' from the first 30 seconds. If you want to move backward and forward
#' within a session, use \code{replay(expt, folder="xxx")} where xxx is the specific 
#' session of interest. If you don't want to create a new folder - or if you 
#' want the session name to be preserved, e.g. for crash recovery during a live
#' experiment - then use \code{rewind=TRUE}.
#' 
#' @examples
#' \dontrun{
#' # error recovery:
#' start(expt)
#' # something goes wrong after 2 minutes
#' # run the first two minutes, then check each step 
#' rewind(expt, ask=120)
#' # now refresh your clients and continue
#' 
#' # testing & debugging 1:
#' replay(expt, ask=TRUE) # watch your data after every request
#' 
#' # testing & debugging 2:
#' ready(expt)
#' replay(expt, live=TRUE, speed="realtime", clients=1:3)
#' # play live as client 4 yourself
#' }
#' @family command line functions
#' @export
replay <- function(experiment, folder=NULL, maxtime=Inf, speed=NULL, ask=FALSE,
  live=FALSE, clients=NULL, rewind=FALSE) {
  experiment$replay(folder=folder, maxtime=maxtime, speed=speed, ask=ask, 
    clients=clients, live=live, rewind=rewind)
}

#' @export
#' @rdname replay
rewind <- function(...) replay(..., rewind=TRUE, folder=NULL)

#' Trace one or more experiment stages
#' @param experiment an object of class Experiment
#' @param num numbers of stages to trace
#' @param ... arguments passed on to the \code{trace} method
#'        of the Stage object. See \code{\link{setRefClass}}.
#' 
#' @examples
#' \dontrun{
#' trace_stage(expt, s1, browser)
#' }
#' @family command line functions
#' @export
trace_stage <- function(experiment, num, ...) for (n in num) 
  experiment$stages[[n]]$trace("handle_request", ...)

#' @rdname trace_stage
#' @export
untrace_stage <- function(experiment, num) for (n in num) 
  experiment$stages[[n]]$untrace("handle_request")
