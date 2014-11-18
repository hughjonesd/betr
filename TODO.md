BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [] Server doesn't stop after rm(expt). Still there?
- [] RookServer creates sink() a lot
- [] FormStage doesn't like not getting params...?
- [] how to move on manually from TextStages? autorefresh?
- [] ReplayServer gets put in .oldserver
- [] documentation doesn't show default values e.g. in experiment(...)
- [] ?? ready isn't reinitialized in Programs on replay?

TODO
----
- [x] timeout stage to wrap other stages
  - would be nice to do this for "wait" also but hard to "redisplay the page"
- [] use makeActiveBinding to "watch" data frames etc.... or define
  an on_request hook?
- [] easy equivalent to zTree Participate=0, i.e. make some subjects skip a stage
- [x] general framework for HTML pages
  - [] how to deal with 'errors'?
- [] check function errors should be translatable
- [] ways to have arbitrary variables in (different) stages, e.g. for 'brew'
- [] simple way to make multiple, slightly different stages (e.g. copy +
  interface to various object fields)?

- [] is_email, is_date
- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [] vignette: tips/tricks/bugs
- [] vignette: using multiple parameters

Other thoughts
--------------
- HTML form checks could be expressions: `is_numeric(x) && x %in% 1:10`
- with error messages in separate array?
- make matching and within-group calculations simple, e.g. 
  `group`, `role` primitives?
  - The dplyr way: ultimatum game.

```splus
subject %<>% group_by(pair) %>% mutate(
      accepted=accept[role==1] <= offer[role==2], 
      payoff=accepted*ifelse(role==1, 10 - offer, offer))
```

- pass in code, not just functions, with id, period, params etc. in environment
  - use `substitute` and a decent package for evaluation
- put builtin data in "tidy" form: subject/period, subject, period, global?
  - but what if it's hard to merge things?
- data frame "views" in some way... separate package?
- what if you want to ask someone the same Q repeatedly (e.g. eliciting more
reasons)

Next iteration
--------------
- [] chat box with Rook/websockets?
- [] media server with Rook?
- [] JSON server with Rook?
- [] dynamics with Shiny?
- [] package for updatable views in R?
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

