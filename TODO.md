BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [x] Server doesn't stop after rm(expt). Still there?
- [x] RookServer creates sink() a lot
- [] how to move on manually from TextStages? autorefresh?
- [x] ReplayServer gets put in .oldserver
- [] help pages don't show default values e.g. in experiment(...)
- [] ?? ready isn't reinitialized in Programs on replay?
- [] empty strings in params become NULL on replay (e.g. for namecheck in expt1)
- [] issues when parameters don't exist; we should be relaxed about this
- [] next_stage() through a Period does not increment period
  - this is really an architectural bug. We don't differentiate between
  "doing calculations" and "getting input from the user". If we advance a stage
  we want to do calculations, but not bother showing input to the user. 
  More deeply, we don't have "push". So when the admin pushes users forward
  a stage, nothing happens till they hit refresh...

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
  - needs to be on a per-person basis. Maybe just within the data frame?
- [] simple way to make multiple, slightly different stages (e.g. copy +
  interface to various object fields)?
- [x] way to go to a particular period in `replay`, and to start live from
  there
- [] is_email, is_date
- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [] vignette: tips/tricks/bugs
- [] vignette: using multiple parameters
- [] wiki

To document
-----------
- [] new betr-data file path

Plan for stages separating actions from response to users
---------------------------------------------------------

- Unnecessary, if we get server push. Because then,
  a new stage is always run when next_stage is called.

Other thoughts
--------------
- HTML form checks could be expressions: `is_numeric(x) && x %in% 1:10`
  - with error messages in separate list, using gettextf?
  - and so could stages, with default values for e.g. id, period...
- maybe replace `print_stages` with `stages` which returns a list with
  a `print` method?
- make matching and within-group calculations simple, e.g. 
  `group`, `role` primitives?
  - The dplyr way: ultimatum game.

```splus
subject %<>% group_by(pair) %>% mutate(
      accepted=accept[role==1] <= offer[role==2], 
      payoff=accepted*ifelse(role==1, 10 - offer, offer))
```
  - perhaps we have automatic `role` and `group` fields?
  - and convenience methods for stranger etc. groups?
- pass in code, not just functions, with id, period, params etc. in environment
  - use `substitute` and a decent package for evaluation
- put builtin data in "tidy" form: subject/period, subject, period, global?
  - but what if it's hard to merge things?
- data frame "views" in some way... separate package?
- what if you want to ask someone the same Q repeatedly (e.g. eliciting more
reasons) without a defined endpoint? 
  - Could be a 'stage' that repeated itself until a defined condition
  - NB stage objects currently handle both "page display" (e.g. TextStage
    versus FormStage) and control flow (e.g. Period, Checkpoint)... perhaps
    should be separated out somehow.
- separate Session from Experiment.


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

