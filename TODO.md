BUGS
----

- [] RookServer not respecting port (workaround; needs actual fix)
- [x] Needs this_url()
- [] Last period info of guessing-game is lost
- [] Autostart not working
- [] Server doesn't stop after rm(expt)

TODO
----

* how to allow for different possible Ns, and general framework for when to start
* elegant, easy-to-use assignment to subjects
  - do we merge expt$subjects and experiment's actual data?
  - probably if it is easy...
  - subjects(id) and subjects(id)<- methods?
* in general what if we pass lots of stuff in to user-defined functions
  which just take (...)?

* easy interface to brew

* use addTaskCallback and friends to print something out at end of repl loops?

* replay()
* autocreate fake participants using request records
* More generally, framework for faking participants
  - Say, replay(realtime=1, clients=NA, exclude=NA) 
  - exclude= to exclude one particular client (who you can then fake)
  - client= to include specific clients
  - realtime= means, do up to realtime-1 instantly, and include all clients
    - thereafter include only listed clients, and go in realtime

* rewind(to=NA, back=1, ids=NA)
  - back=1 just calls to=period-1 with period defined by ids
  - to is a recycled vector of periods, usually just length 1, applied to ids
  - idea: restart the experiment, create a new records/ folder
  - replayed commands also create their own records
  - replay everything, but some subjects only get to their "to"...
  - will this always make sense?
    - coord game, 5 stages, on stage 5. rewind(to=1, ids=1). 
      Requests for stages 2-5 of id 2 won't make sense... would they automatically
      result in waiting_page()
  - for the moment just implement rewind(to)... 
    though, is that always guaranteed to be OK? what if we are playing a cascade
    game? might want subject 1 to be making 4th choice while 2 makes 3rd, 3 makes
    2nd, 4 makes 1st. 
  - clearly, if you have time dependencies in your code, rewind will break
    Just as if you alter the filesystem etc...
  - easy to enforce no time dependencies on "future periods": just make sure
    subjects() never returns anything from the future. Then it always makes
    sense to run everyone e.g. "back to period 16".
  - can we also enforce "no dependencies on other subjects"? 
    Perhaps some stages could have an environment where only subject()
    and not subjects() is defined... 
  - More generally, could create a per-period environment in which stages run
    ... but this is a bit BDSM

* RookServer to serve static files (perhaps via separate App?)

* How to easily identify computers in lab (if not by IP address)?
  - create a useful identify_computers() function 
  - serves a simple web app
  - which makes a list of IP address-> computer number
  - and/or sets a cookie on the client browsers and makes a list of cookie -> computer #

* How to have different clients in the same windows of the webbrowser?
  - in development mode, set client_id as a param of the URL 
  - or even like /experiment/client_id ?

* More sophisticated stages.
  - stages with a run and check phase (and maybe setup and teardown)

LATER
-----

* Mturk et al payment integration?
* Command line interface and tools
* Documentation
* JSON server + HTML page... probably using shinyServer
