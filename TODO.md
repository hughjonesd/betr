BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [] Server doesn't stop after rm(expt)
- [] RookServer creates sink() a lot
- [] FormStage doesn't like not getting params...?
- [] how to move on manually from TextStages? autorefresh?
- [] ReplayServer gets put in .oldserver
- [x] problem with timed() in PG game?
- [ ] knit still showing too much output

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
- [] proper readline for replay
- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [x] mergewithsubjects function
- [x] IP address in subjects table
- [] vignette: include graphics with knitr/brew
- [] vignette: using brew
- [] vignette: tips/tricks/bugs
- [] chat box with Rook?
- [] media server with Rook?
- [] JSON server with Rook?
- [] admin interface with Rook or gtk?



OTHER IDEAS
-----------

* Mturk et al payment integration?
* Command line interface and tools
* RookServer to serve static files (perhaps via separate App?)
* JSON server + HTML page... probably using shinyServer

