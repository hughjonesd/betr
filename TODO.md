BUGS
----

- [] RookServer not respecting port under RStudio (workaround; needs actual fix)
- [] Server doesn't stop after rm(expt)
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
- [] proper readline for replay - not now, seems hard!
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
- [] chat box with Rook?
- [] media server with Rook?
- [] JSON server with Rook?
- [] dynamics with Shiny?
- [] admin interface with Rook or gtk?
- [] include the subjects data frame in a standard location on a per period basis



OTHER IDEAS
-----------

* Mturk et al payment integration?
* Command line interface and tools
* RookServer to serve static files (perhaps via separate App?)
* JSON server + HTML page... probably using shinyServer

