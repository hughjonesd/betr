BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [x] Needs this_url()
- [x] Last period info of guessing-game is lost
- [x] Autostart not working
- [] Server doesn't stop after rm(expt)
- [] RookServer creates sink() a lot
- [] FormStage doesn't like not getting params...?

TODO
----

- [] use makeActiveBinding to "watch" data frames etc.
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



OTHER IDEAS
-----------

* Mturk et al payment integration?
* Command line interface and tools
* RookServer to serve static files (perhaps via separate App?)
* JSON server + HTML page... probably using shinyServer

