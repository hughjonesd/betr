BUGS
----

- [] RookServer not respecting port (workaround; needs actual fix)
- [x] Needs this_url()
- [x] Last period info of guessing-game is lost
- [x] Autostart not working
- [] Server doesn't stop after rm(expt)

TODO
----


- [] use makeActiveBinding to "watch" data frames etc.
- [] experiment has own time() function to make sure replay gets timeouts right
- [] timeout stage to wrap other stages
  - would be nice to do this for "wait" also but hard to "redisplay the page"

- [x] replay() 
- [] replay max_requests, ids, clients options
- [] try live replay by not halting main server

- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?


OTHER IDEAS
-----------

* Mturk et al payment integration?
* Command line interface and tools
* RookServer to serve static files (perhaps via separate App?)
* JSON server + HTML page... probably using shinyServer

