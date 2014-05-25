BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [] Server doesn't stop after rm(expt). Still there?
- [] RookServer creates sink() a lot
- [] FormStage doesn't like not getting params...?
- [] how to move on manually from TextStages? autorefresh?
- [] ReplayServer gets put in .oldserver
- [x] problem with timed() in PG game?
- [x] knitr options infecting my Rmd files from pages.R!

TODO
----
- [x] replay() 
- [x] try live replay by not halting main server
- [x] use mAB to implement single-word commands (`READY`, `START`, `INFO`)
- [x] run without a record
- [x] experiment has own time() function to make sure replay gets timeouts right
- [x] timeout stage to wrap other stages
  - would be nice to do this for "wait" also but hard to "redisplay the page"
- [] use makeActiveBinding to "watch" data frames etc.... or define
  an on_request hook?
- [x] general framework for HTML pages
  - [] how to deal with 'errors'?
- [] is_email, is_date?
- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [x] mergewithsubjects function
- [x] IP address in subjects table
- [x] vignette: include graphics with knitr/brew
- [x] vignette: using brew
- [] vignette: tips/tricks/bugs

Next iteration
--------------
- [] chat box with Rook/websockets?
- [] media server with Rook?
- [] JSON server with Rook?
- [] dynamics with Shiny?
- [] admin interface with Rook or gtk?
- [] include the subjects data frame in a standard location on a per period basis
  - integrate existing subjects df; also Profit, Group?
  - `subject(ids, periods)` and `subject(ids, periods, ...)` functions
  - similarly `group(gids, periods)` and `subjects(periods)`
  - all with defaults taken from current id, period? This requires
  messing with environments, see below. Problem: approach won't work
  reliably when called from other functions.... Seems extra hassle compared
  to just typing `subject(i, p, ...)` or `group(group_of(i,p), p, ...)`.
  
```splus
  tmp <- setRefClass("tmp", 
    fields=list(id="ANY", period="ANY", handler="ANY", mydf="data.frame"), 
    methods=list(
      callMe = function() {
        assign(".id", id, pos=environment(handler)); 
        assign(".period", period, pos=environment(handler)); 
        assign("subject", function(id=.id, period=.period, a){
              mydf[mydf$id==id & mydf$period==period,"a"] <<- a}, 
              pos=environment(handler)); 
        handler(id, period)
      }
    ))
```

- [] MTurk payment integration?


ShinyExperimentServer plan
--------------------------

* Require Shiny server to be installed....
* SES knows about ShinyServer (or even is a subclass?)
* server.R includes shinyServer(func) where func is part of SES
  - or you can use runApp... but NB, does not return! parallel?
* HTML output goes into a central page
* No more Rook objects: timeouts set as attribute, detected by Server
  - reactiveTimer() could be the implementation
* NEXT, WAIT and next_stage immediately pushed to client
* new stages which presumably return a basicPage object
  - Pointless to pass these to RookServer; give them shiny_ prefix
* existing stages get wrapped in HTML() and pushed to client by SES
* header() and footer() delegated to Server?
* SES registers UI handler via shinyUI
* need to make sure that anything that changes experiment state is handled
  as a request. maybe a function that wraps the various UI elements and says
  "this one generates a request"?
* some new shiny objects showing per-user data?

