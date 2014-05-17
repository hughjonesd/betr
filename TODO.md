BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [] Server doesn't stop after rm(expt)
- [] RookServer creates sink() a lot
- [] FormStage doesn't like not getting params...?
- [] how to move on manually from TextStages? autorefresh?


TODO
----

- [] use makeActiveBinding to "watch" data frames etc.... or define
  an on_request hook?
- [x] run without a record
- [x] experiment has own time() function to make sure replay gets timeouts right
- [x] timeout stage to wrap other stages
  - would be nice to do this for "wait" also but hard to "redisplay the page"
- [x] replay() 
- [x] try live replay by not halting main server
- [] proper readline for replay
- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [] vignette: include graphics with knitr/brew
- [] vignette: using brew
- [] vignette: tips/tricks/bugs
- [] chat box with Rook?
- [] media server with Rook?



OTHER IDEAS
-----------

* Mturk et al payment integration?
* Command line interface and tools
* RookServer to serve static files (perhaps via separate App?)
* JSON server + HTML page... probably using shinyServer

