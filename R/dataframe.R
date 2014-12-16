
#' @import refset
#' @import R6
# plan:

# subjects = id
# session = id, period
# clients = old subjects?
# group = id [only group]
# subject = id [just one row; still a data frame?]
# globals = just one row; still a data frame
# session_globals = period
# period = l1 numeric
# id = l1 numeric

# nothing in the global environment, but you can return a refset
# by calling the appropriate function?
# or, readonly copies in the global environment?

# what to do if N changes over periods?
# perhaps if we don't even know what N will be?
# e.g. web experiment, new guys arrive and are sequentially
# put through the experiment
# so:
# time 1
# period  1   2   3
# id      1
# time 2
# period  1   2   3
# id      2,3 1
# time 3
# period  1   2   3
# id      2,3     1
# time 3
# period  1   2   3
# id      2   3   1

# we can still assume that all subjects go through all periods eventually,
# so we can still create rows. but we may need to add rows when new subjects
# turn up, while there is existing data
# and we may want to initialize certain values...
# but perhaps that is a job for a program().
# so, we need a "new_subject()" method.

# what to do if there are irregular sized groups?

# strategy 1:
# - groups are predefined
# - N must be an integer multiple of group size
# strategy 2:
# people are randomly allocated into a group
# strategy 3: groups change every period, but are predefined
# 4: some kind of endogenous process...

Datastore <- setRefClass("Datastore", fields=list(
    session="data.frame",
    session_globals="data.frame",
    clients="data.frame",
    subjects="ANY",
    group="ANY",
    subject="ANY",
  ),
  
  methods=list(
    initialize=function(...) callSuper(...),
    
    create_data=function(N=0, T=0, profit=0, group=NA_character_, ...) {
      period <- rep(seq_len(T), each=N)
      id <- rep(seq_len(N), T)
      session <<- data.frame(period=period, id=id, profit=profit,
            group=group, ..., stringsAsFactors=FALSE)
      session_globals <<- data.frame(period=seq_len(T))
    },
    
    assign_variables=function(env) {
      refset(session, session[], assign.env=env, eval.env=.self)
      refset(session_globals, session_globals[], assign.env=env, eval.env=.self)
      refset(subjects, session[session$period==period,], assign.env=env, 
            eval.env=env)
      refset(subject, session[session$period==period & session$id==id,], 
            assign.env=env, eval.env=env)
      refset(group, session[session$period==period &
            session$group==subject$group,], assign.env=env, eval.env=env)
      refset(globals, session_globals[session_globals$period==period,], 
            assign.env=env, eval.env=env)
    },
    
    write_data=function(dir) {
      write.csv(session, file=file.path(dir, "session.csv"), header=TRUE, 
            row.names=FALSE)
      write.csv(session_globals, file=file.path(dir, "globals.csv"),
            header=TRUE, row.names=FALSE)
      write.csv(clients, file=file.path(dir, "clients.csv"),
            header=TRUE, row.names=FALSE)
    }
  )
)
