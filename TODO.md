

Plan
----


## 1. Basic framework using shiny.

### requirements
- easy for experimenters to define forms, take actions, define complex
  flow through stages (including on the fly)
- easy to create a "shiny page" which uses whatever shiny offers
- must be implementable using Shiny server (free version)
- and preferably also from shinyapps.io
- must not require experimenters to immerse themselves in Shiny
- must allow server-push for stages, timeouts etc.

### plan
- do a simple PG game in Shiny
- use the knowledge to design new framework

### issues:
- Typically, HTTP headers sent to Shiny Server will not be forwarded to the 
underlying Shiny application. (from the administrator's guide). So, how
get IP address for auth?
- Doesn't seem possible to connect to the R session in Shiny Server; or to run 
shiny apps daemonized from the command line. (But maybe can hack it to use 
httpuv's startServerDaemonized?)
      - Does it matter ie should we anyway have a "management console"?
      - https://github.com/trestletech/shinyAce is an integrated editor in shiny
          - has an "eval" example where code is evaluated
      - for an example of a complex ui, with drag n drop etc.,
  see https://github.com/iheartradio/ShinyBuilder 


### ideas:
- separate experiment definition from session running
- separate "policy" and "mechanism" layers
- functions for matching groups
- event-driven approach, e.g. events for
    - subject reached new stage
    - subject submitted form
    - subject timed out
    - new period
    - maybe these are listened to via reactive() framework?
    - stage$onFirstEntry, $onSubjectEntry, $onTimeout, ...
- shiny has validate() and need() for input
- use d3 etc for charts, rather than doing it on the server side
- "as much as possible, program the logic for the whole group together"
    - lots of dplyr subjects %>% group_by(group) ...
    - another useful trick, with pryr: dynset %<a-% rs %>% group_by(...) %>% ...
    - if we decouple code from "this user got here", then it's more natural
    not to rely on a particular user id...
    - effectively so far our events have been "this user got to this stage",
    and "this user submitted this input". We can be broader than this
- define some easy-to-use pages as shiny UI elements:
    - textPage
    - formPage (with autofill to/from database and with form checking)
    - questionnaire (a kind of formPage)
- this seems relevant: https://github.com/jcheng5/shiny-partials


## 2. Robots

### requirements
- subjects can be real or robots
- robots can be created automagically from a past session (matches params to
stages)
- and should be a nice way to edit them (e.g. DSL?)
- able to react to specific html?
- code chain looks like: page passed to "user" which _either_ 
    - is a proxy that passes it back to the web server (or shiny...) for rendering,
    - _or_ is a robot which decides what to do based on the page
  
Plan for stages separating actions from response to users
---------------------------------------------------------

- Unnecessary, if we get server push. Because then,
  a new stage is always run when next_stage is called.

Random thoughts and TODOs
--------------- 

- [] next_stage() through a Period does not increment period
  - this is really an architectural bug. We don't differentiate between
  "doing calculations" and "getting input from the user". If we advance a stage
  we want to do calculations, but not bother showing input to the user. 
  More deeply, we don't have "push". So when the admin pushes users forward
  a stage, nothing happens till they hit refresh...
- [] use makeActiveBinding to "watch" data frames etc.... or define
  an on_request hook?
- [] easy equivalent to zTree Participate=0, i.e. make some subjects skip a stage
- [] check function errors should be translatable
- [] ways to have arbitrary variables in (different) stages, e.g. for 'brew'
  - needs to be on a per-person basis. Maybe just within the data frame?
- [] simple way to make multiple, slightly different stages (e.g. copy +
  interface to various object fields)?

- [] HTML form elements in separate package
  - checker functions
  - javascript attribute?
  - using Twitter Bootstrap?
- [] wiki

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

