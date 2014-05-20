
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{A tutorial on writing and running experiments}





Writing experiments with betr
=============================

Rationale
---------


betr is an R package to design, test and run social scientific experiments. It 
uses the web to serve experiments, but it is suitable either for web- or 
lab-based experiments. It is designed to make writing, testing, debugging and 
running experiments as easy as possible.

## Why use betr?

Typically, social science experiments are run using computer software. The most
widely used software by economists is [zTree](www.iew.uzh.ch/ztree/). Others 
are written in Java, Python, or PHP. So, why use betr? 

The most important advantageis:

* betr is written in R, so you don't need to learn a new programming language
to use it.

That is true if you know R already. If you don't know R, but are an early-career
social scientist, then you probably need to learn at least one statistical 
software package. If you learn R, then you will also be able to use it to run
experiments in betr.

Some other advantages are:

* Experiments can be written in a full-featured programming languages.

While zTree's language lacks constructs such as functions, with betr you can use
all the features of R. _No more copy-pasting the same code across different periods._
R built-ins are also useful for writing experiments. Here's a standard way
to randomize over your subjects in zTree:

```
subjects.do{
  randomizer = random();
}
subjects.do{
  rand_id = count ( randomizer >= : randomizer );
}
```

The same thing in R is just:


```splus
rand_id <- sample(1:N)
```


* betr uses HTML to display experiments to subjects.

HTML is a powerful and flexible display language, so your experiments can be as
beautiful or as simple as you like.

* Debugging and testing is easy.

Testing experiments by hand can be a pain. Typically, you have to open one window for
each of your subjects, then type in responses manually. betr lessens that pain
by allowing automatic replay of sessions. So, you can test manually once, then
rerun the test automatically. 

* You can run experiments on the web or in the lab.

Obviously, since betr uses HTML, you can serve web-based experiments. In your
lab, you only need a web browser on client machines.

* Error recovery is easy and safe.

Things can go wrong in the lab. In a betr session, all client and experimenter 
interactions are stored on disk, so even if your server computer crashes, you can 
replay back to where you left off and restart the experiment.

* No need for session management, databases, cookies, etc. 

Languages like PHP are designed to respond to a single request, perhaps store some
information in a database, print out a 
web page, then die. When they get a new request, they start again, look
for session information to remember where they were, get info from the 
database.... This does not really suit the paradigm of an experiment where you 
want users to go through steps simultaneously in a carefully defined order. 
The resulting session and database management can be a hassle. 

betr takes a different approach: a single persistent server process handles all 
requests from start to finish.
There's no need to talk to a database: data is held in memory. At any time you 
can see exactly where your subjects are, using simple R commands.
And your experiment can be written in a single source file. 

Installing betr
---------------

To install betr just run:


```splus
install.packages("devtools") # if not already installed
library(devtools)
install_github("betr", "hughjonesd") # for the latest version
```


An example betr experiment
--------------------------

Typically you will define your experiment in a source file. Then, during a 
session, you will run it from the command line. Here's a source file for a
simple guessing game experiment.


```splus
library(betr)

initialize_data_frame <- function() {
  mydf <<- experiment_data_frame(expt, guess=NA, correct=NA)
}
expt <- experiment(N=1, clients_in_url=TRUE, on_ready=initialize_data_frame,
      seats_file=NULL)

s1 <- function(id, period, params) {
  me_now <- mydf$id==id & mydf$period==period
  if ('guess' %in% names(params)) {
    mydf$guess[me_now] <<- as.numeric(params$guess)
    mydf$correct[me_now] <<- if (params$guess == sample(1:10,1)) 1 else 0
    return(NEXT)
  }
  last_guess <- ''
  me_before <- mydf$id==id & mydf$period == period -1
  if (period > 1) last_guess <- paste0('You guessed ', mydf$guess[me_before], 
        '... you were ', if(mydf$correct[me_before]>0) 'right!' else 'wrong!')
  return(c(header(), sprintf("<p color='red'>%s</p>", last_guess),
    "<p>Pick a number</p>",
    "<form action='' method='post'><select name='guess'>",
    paste0("<option value='", 1:10,"'>", 1:10, "</option>", collapse=""),
    "</select>",
    "<input type='submit' value='Submit'></form>",
    footer()))
}

add_stage(expt, period(), s1, times=5)
```


Let's look at this bit by bit.

Our first task is to prepare a data frame for our experiment. We could just 
do this by calling


```splus
mydf <<- data.frame(id=1, period=1:5, guess=NA, correct=NA)
```


However, we want to make sure that our experiment is _replay-safe_. If we 
replay our experiment, either during testing or to recover from a problem in a
session, our experiment will be rerun from the start. But if our data has 
already been written, then this might affect how things work. To make sure
that replay will work exactly the same, we create a function 
`initialize_data_frame` that creates a fresh empty dataset. This
function will be called whenever the experiment is made ready, including
when it is replayed. Then the replay will populate our data frame just as it was.

`experiment_data_frame` is a convenience function. It sets up a
data frame with the right number of rows and columns for our experiment -- one
per subject per period. 

Next we set our experiment up. The call to `experiment` returns an object of 
class Experiment, which we have called `expt`. `N=1` gives the number of 
participants, and `on_ready=initialize_data_frame` tells betr to call our 
function whenever the experiment is made ready. Don't worry about the other 
options for now.

 In betr, experiments are composed of
_stages_. The next piece of code defines a function `s1`, which will be one of 
our experiment's stages. Ignoring the details, the structure of `s1`
is like:


```splus
s1 <- function(id, period, params) {
  # ...
  return(NEXT)
  # or ...
  return("some HTML")
}
```


Here's how this works. When a subject makes an HTTP request to betr, the current
stage is called with the subject's `id` (a number from 1 to N), the
subject's current `period`, and any parameters from the request -- e.g.
data from a form filled in by the subject. The stage can then do one of three 
things:

1. Return some HTML to the subject.
The subject stays on the same stage. For example, if you return
an HTML form, the same stage will be called when the subject fills in the form
and hits _Submit_.
2. Return the special constant `NEXT`.
The subject is then advanced to the next stage, which is immediately
called without parameters.
3. Return the special constant `WAIT`.
The subject is shown a "waiting page". After a defined
time interval, this will autorefresh and the stage will again be called.

In `s1`, the stage looks at whether there is a parameter called `guess`. If there
is, the stage records the subject's guess in the data frame `mydf`. It also
decides whether the subject guessed right, and records this in `mydf$correct`. 

* Notice one difference with zTree: whereas zTree records all data in tables called
_subjects_, _globals_ etc., in betr you are free to record your data how you like.
A typical way is to create a data frame with 1 row per subject per period.

* Notice also that `mydf` is assigned to using the operator `<<-` rather than
the more usual operator `<-`. This makes sure that assignment happens in the 
global environment, rather than just in the function's local environment.

Having recorded the data, `s1` returns `NEXT` and moves the subject on. If we
aren't finished, `s1` will then be called again without parameters. `s1` will
then print out the guessing form so the subject can choose another number.

Lastly, we add our stage `s1` to the experiment:


```splus
add_stage(expt, period(), s1, times=5)
```


What about the other thing here, `period()`? In fact, `period()` creates another
kind of stage, called (guess what?) a Period. This stage is very simple: it 
adds 1 to the subject's period counter, then returns `NEXT`. 

The `times=5` argument repeats the whole sequence of stages 5 times. So
our experiment's stages are now:

> period, s1, period, s1, period, s1, period, s1, period, s1

OK, we've created our experiment. Now, we need to run it. Run the code above, or
put it in a file called `my_experiment.R` and source it:


```splus
source("my_experiment.R")
```


Before we run it, let's take a look at it. On the command line, type

```splus
expt
```

```
## Name: betr	Status: Stopped	Clients: 0/1	Periods: 5	Stages: 10
```


This gives you basic information about the experiment. _Clients_ tells you how
many clients have connected, out of the experiment's N. _Periods_ and 
_Stages_ are self-explanatory. There are 5 periods because our experiment
had 5 periods in its stages. There are 10 stages including the 5 periods and
the 5 `s1` objects. Lastly, the _Status_ tells us whether the experiment is 
_Stopped_, _Waiting_, _Started_ or _Paused_. Right now it is _Stopped_. Let's
change that. On the command line, run:


```splus
ready(expt)
```

```
## Loading required package: tools
## starting httpd help server ... done
```

```
## 
## Server started on host 127.0.0.1 and port 35538 . App urls are:
## 
## 	http://127.0.0.1:35538/custom/betr
```


Typically, you will call `ready` when you want to start connecting subjects to
the experiment. `ready` does several things:
* Starts the web server so that subject computers can connect to the experiment;
* Creates a new experiment session, with a date and time, and creates a folder
on disk to hold data about it;
* Calls any function passed to `on_ready`, e.g. initializing your data.

Now, `expt` will report more information:


```splus
expt
```

```
## Session: betr-2014-05-20-232616	Status: Waiting	Clients: 0/1	Periods: 5	Stages: 10
## Serving at http://127.0.0.1:35538/custom/betr
```


The status has changed, and we see the session name. We also have a URL. Clients
can connect to this to view the experiment. You can do this manually: open your 
web browser and go to the URL http://127.0.0.1:35538/custom/betr/client-1. Or,
for a convenient shortcut, enter:





```splus
web_test(expt)
```


on the R command line. You should see a page saying "Waiting to start". It will 
refresh regularly. (If you get an error message, check your computer's firewall 
settings. You may need to allow access to certain ports.)

Before we start, let's see a bit more information about our experiment. Type:


```splus
info(expt)
```

```
## Session: betr-2014-05-20-232616	Status: Waiting	Clients: 1/1	Periods: 5	Stages: 10
## Serving at http://127.0.0.1:35538/custom/betr 
## Subjects:
##     client IP id seat period stage  status
## 1 client-1 NA  1   NA      0     0 Running
## Period progression:
## 0: . [1]
```


This shows the same info as before, plus a list of subjects -- just one.

Now we've got our 1 subject, we're ready to start. On the command line, type:


```splus
start(expt)
```





Now, when your browser page refreshes, you will see the first period of the 
guessing game. Complete a couple of guesses (good luck!). Use `info(expt)`
to watch your subject progressing. You can also look directly at your 
experimental data frame:


```splus
mydf
```

```
##   id period guess correct
## 1  1      1     6       0
## 2  1      2     2       0
## 3  1      3    NA      NA
## 4  1      4    NA      NA
## 5  1      5    NA      NA
```


At the end, you will see an "experiment finished" page in your browser, and
`info(expt)` will show that your subject has finished the experiment.



To stop the experiment serving, run


```splus
halt(expt)
```


Now the server will halt, so clients can no longer connect, and the status
of the experiment will be "Stopped". 

Our experiment is finished, but our data is still there, as you can see if you
type `mydf`. To save it to a CSV file, run:


```splus
write.csv(mydf, file="my-first-data.csv")
```

Lastly, let's replay our experiment. Enter


```splus
replay(expt, ask=TRUE)
```


The command prompt should show 

```
replay>
```

Enter 'd' to see details of the first request from a client computer. Then enter
'n' to replay this request. Carry on entering 'd' and 'n'; eventually you will 
see your 'start' command. Enter 'n' three more times. Now enter `mydf`. You
will see that your data frame is back as it was in the middle of the experiment.
You can enter arbitrary R expressions. For a list of other commands, enter 
'h' or '?'.

Writing experiments
-------------------

### Experiment parameters

So far our experiment is rather trivial: guessing a random number. Let's change
it to have multiple subjects, each matched in groups, and getting paid if they 
all choose the same random number -- i.e., a coordination game. By doing this,
we'll learn about the different kinds of Stages. So
far you've met two kinds: simple functions which return `NEXT`, `WAIT` or some
HTML; and Period objects created using `period`. 

We'll set up our data frame much as before:


```splus
library(betr)
N <- 8
nreps <- 4
groupsize <- 2
reward <- 5 # reward in $

if (N %% groupsize > 0) stop("N must be an exact multiple of groupsize")
Ngroups <- N/groupsize


initialize <- function() {
  mydf <<- experiment_data_frame(N=N, periods=nreps)
  mydf$guess <<- NA
#   mydf$correct <<- NA
  mydf$group <<- rep(rep(1:Ngroups, each=groupsize), nreps)
}

expt <- experiment(N=N, clients_in_url=TRUE, on_ready=initialize,
      seats_file=NULL)
```


Note that:

* Parameters for the experiment are set at the top of the file so they can be
edited easily.

* The column `mydf$group` records each subject's group. In this case, subject ids
1-2 are in group 1, subject ids 3-4 are in group 2, and so on. This is not
randomized, but by default, betr randomizes IDs across subjects, so we are fine. 
If you wanted to explicitly randomize you could just add the following line to 
`initialize`:


```splus
mydf$group <<- sample(mydf$group)
```


If you wanted to redraw groups each round, you could do something like:


```splus
groups <- rep(1:Ngroups, each=groupsize)
for (i in 1:nperiods(expt)) mydf$group[mydf$period==i] <<- sample(groups)
```


* Notice as before that when we assign to `mydf`, we use the global assignment 
operator `<<-`.

### Text Stages

Let's add some instructions to our experiment. To do this, we'll use a TextStage
object.


```splus
ins <- text_stage(page=c(
  header(), 
  "You will be matched in groups of size", groupsize,
  ". If you each guess the same number, you will get a reward of $", reward,
  ". <form action=''><input type='Submit' value='OK'></form>",
  footer()
))
add_stage(expt, ins)
```


`text_stage` creates very simple stages: they display some text to the user, then
return `NEXT`. At the bottom of our text, we add an HTML form with a single submit
button. When the subject clicks this, he or she will move on to the next stage.

The functions `header()` and `footer()` create some simple HTML to start and
finish the web page. They are there for convenience. 

We don't always want the user to be able to move on. Sometimes we would rather 
make them wait until the experimenter moves them on. Doing this is simple:


```splus
ins2 <- text_stage(page=c(
  header(), 
  "Please wait for the experiment to begin!",
  footer()
), wait=TRUE)
add_stage(expt, ins2)
```


Now the user can't move on because there is no submit button. In addition,
even if the browser were refreshed, the text will simply redisplay because
of the `wait=TRUE` option. To move all subjects on manually from the command
line:


```splus
next_stage(expt, 1:N)
```


Here `1:N` gives the subject ids as shown by `info(expt)`. Moving subjects
on manually might risk losing data, but as this stage is just displaying some 
text, we're fine.

### Form Stages

Next we need to let subjects pick a number. Last time we did this with a 
function which either displayed an HTML form, or stored the subject's guess.
We can do this even more simply using a new kind of Stage: a FormStage object,
created by the `form_stage` function.


```splus
myform <- c(
  header(), "<p style='color:red;'><% errors %></p>",
  "<p>Pick a number:</p>",
  "<form action='' method='POST'><select name='guess'>",
  paste0('<option>', 1:10, '</option>'), 
  "</select>",
  "<input type='submit' value='Submit'></form>", 
  footer()
)
guess_s <- form_stage(page=myform, fields=list(
      guess=all_of(is_whole_number(), is_between(1, 10))
      ), data_frame="mydf")
```


A form stage prints out an HTML form for the subject. When the form is submitted,
it is checked for errors. If there are no errors, the corresponding fields
in the database are updated. You specify which fields to update by the list
`fields`. The _names_ of this list are names of form fields, which should also
be columns in your data frame. Each _value_ in the list should be a function to check
the user-submitted data. You can write your own functions, but here we have
auto-generated one with `is_whole_number`, `is_between` and `all_of`. (These are
functions which return functions!) So, here we require the `guess` parameter to be
_both_ a whole number, _and_ between 1 and 10. 

If there are errors in your fields, the form will be redisplayed. For convenience,
the string "<% errors %>" will be replaced by a list of errors. 

Assuming that there are no errors, the data frame named in `data_frame` is
updated. Note that:

* `data_frame` should be the name of the data frame, not the data frame itself -- i.e.
  `"mydf"` not `mydf`. 
* The data frame must exist in the global environment
* The data frame is expected to be in the standard form returned by 
  `experiment_data_frame`, that is:
  
id | period | ...
---|--------|----
1 | 1 | ...
1 | 2 | ...
... | ... | ...
2 | 1 | ...
2 | 1 | ...
... | ... | ...
* Form parameters are character vectors and will be stored in the database as
  such (even if you've checked them with e.g. `is_whole_number()`). 
  
### Checkpoints

If we have multiple users in groups, we can't let them all run through the 
experiment at their own speed. They have to wait for each other. For this we need 
another kind of Stage called a CheckPoint.


```splus
initialize() # creates mydf so we can use mydf$group
grp <- mydf$group[mydf$period==1]
grp <- grp[order(mydf$id[mydf$period==1])]
check_s <-checkpoint(wait_for=grp)
```


CheckPoints simply hold subjects at a waiting page until some subjects have also
arrived at the checkpoint. You can specify which subjects to wait for by the 
`wait_for` argument to `checkpoint`. This can be `"all"` to wait for all subjects, or 
`"ever"` to wait forever. (This is useful if you want to let the experimenter 
move on subjects manually using `next_stage`.) Or, as in our example, it can be 
a vector of group names, sorted by id. When this happens, each subject will wait 
until everyone in his or her group has arrived. For example, if 


```splus
wait_for=c("a", "b", "a", "b", "c")
```


then subject IDs 1 and 3 will wait for each other, subject IDs 2 and 4 will wait
for each other, and subject ID 5 will not wait for anyone.

### Programs 

After each group has arrived at the checkpoint, we know that they have filled in
their guess. Now we can calculate if they've coordinated correctly. We'll use
a Program stage for this. A Program simply runs some code at a particular point
in the experiment. It doesn't display anything to the subjects -- they just move
on to the next stage.


```splus
calculate_profit <- function(id, period) {
  mydf$guess <<- as.numeric(mydf$guess)
  me_now <- mydf$id == id & mydf$period == period
  
  mygroup <- mydf$group[me_now]
  myguesses <- mydf$guess[mydf$group == mygroup & mydf$period == period]
  profit <- if (min(myguesses) == max(myguesses)) reward else 0
  mydf$profit[me_now] <<- profit
  
}
calc_s <- program(run="all", fn=calculate_profit)
```


Here `calculate_profit` does just what it says. Note that the first line turns
`mydf$guess` into a numeric variable. Also, note how the first and last lines
use `<<-` to assign into `mydf` in the _global_ environment.

The `run="all"` argument runs the program once for every subject. In effect,
profit is calculated several times for every group, but the calculation doesn't 
change so it doesn't. Other values include `"first"` and `"last"`. These run
the program only when the first subject arrives, and only when the last subject
arrives, respectively.

### Putting it together

Lastly, we'll add our stages to the experiment - not forgetting a period counter
using `period()`.


```splus
add_stage(expt, period(), guess_s, check_s, calc_s, times=nreps)
```


The basic pattern here is: form stage, checkpoint, program. You can use this 
simple pattern in many experiments.

### Adding timeouts

Often in lab experiments you want to give subjects only a fixed time to
answer a question, view instructions etc. In betr, you can do this by adding
timeouts to stages. The syntax is like:


```splus
timed(stage, timeout=60)
```


where `stage` is the original stage, and `timeout` gives the number of seconds
before the stage times out. `timed` creates a new stage object of class Timed. 
So, for example, if we wanted to give subjects 30 seconds to guess a number , we
could write:


```splus
add_stage(expt, period(), timed(guess_s, 30), check_s, calc_s, times=nreps)
```


Timeouts work by adding a `Refresh:` header to the http request. Client browsers
will automatically refresh after the timeout is called. If the timeout expires,
then the Timed stage returns `NEXT`. You may want to do something extra in this
case, like set some default values, or record that the subject timed out. You
can do this by adding an `on_timeout` argument to `timed`.


```splus
timed(stage, timeout=60, on_timeout=function(id, period) {
  mydf[mydf$id==id & mydf$period==period, "timed_out_on_me"] <<- TRUE
})
```


### Dynamic pages and images

So far we have only shown the user static pages of HTML. That is rather limited.
We also want to customize the HTML to print out e.g. the past history of play.
We might also want to use R's powerful graphics facilities. 

Actually, you have already seen one way to print HTML dynamically, which is 
just to use a `stage` function. For example, here's how to print out the 
subject's name, assuming they have already submitted it:


```splus
stg <- function(id, period, params) {
  c(header(), "Your name is:", mydf$name[mydf$id==id & mydf$period==period],
        footer())
}
```


You can do something very similar within a `text_stage` or `form_stage`. Instead
of passing a character vector of HTML to the `page` argument, pass a function:


```splus
ts <- text_stage(page=function(id, period, params, errors) {
  c(header(), "Your name is:", mydf$name[mydf$id==id & mydf$period==period],
        footer())  
})
```


The `params` and `errors` parameters will never be used in a text stage, since 
the stage prints only once after the last stage called `NEXT`. However, your 
function should always have them as arguments. 

In a form stage you can do the same thing. The `params` argument will be a
list of the user-submitted parameters. It will be empty when the form is displayed
for the first time, but will have elements if the form redisplays because of user
errors. `error` is a named vector of error messages from form submission.
For example, the following prints out a simple HTML form, keeping user's previous
inputs and displaying any error messages:


```splus
myfun <- function(id, period, params, errors) {
  name <- if ('name' %in% names(params)) params$name else ''
  age <-  if ('age' %in% names(params)) params$age else ''
  html <- header()
  if (length(errors) > 0) html <- c(html, 
        "<div style='color:red; border: 1px solid red;'>",
        paste(errors, collapse="<br />"), "</div>")
  html <- c(html, 
        "<form action='' method='POST'>",
        sprintf("<h1>Period %s: enter your details</h1>", period),
        sprintf("<p>Name: <input type='text' name='name' value='%s'></p>", name),
        sprintf("<p>Age: <input type='number' name='age' value='%s'></p>", age),
        "</form>", 
        footer())
  return(html)
}

fs <- form_stage(page=myfun, fields=list(
        name=has_value(), 
        age=is_between(18,110)
      ), data_frame="mydf")

# test how this works:
myfun(id=1, period=3, params=list(name='John', age='19'), errors='')
```

```
##  [1] "<html><head><title>Experiment</title></head>\n        <body style='background-color: #CCCCCC; padding: 2% 4%;'>\n        <div style='background-color: white; padding: 3% 3%; \n        border: 1px solid #888888; border-radius: 10px;'>"
##  [2] "<div style='color:red; border: 1px solid red;'>"                                                                                                                                                                                          
##  [3] ""                                                                                                                                                                                                                                         
##  [4] "</div>"                                                                                                                                                                                                                                   
##  [5] "<form action='' method='POST'>"                                                                                                                                                                                                           
##  [6] "<h1>Period 3: enter your details</h1>"                                                                                                                                                                                                    
##  [7] "<p>Name: <input type='text' name='name' value='John'></p>"                                                                                                                                                                                
##  [8] "<p>Age: <input type='number' name='age' value='19'></p>"                                                                                                                                                                                  
##  [9] "</form>"                                                                                                                                                                                                                                  
## [10] "</div><div align='center' style='padding: 10px 10px;'>betr</div></body></html>"
```


### Brew and knitr

Using functions which mix R and HTML can start to look rather messy, as the code
above shows. A nicer solution is to use a templating package. This means
you create your HTML pages in a separate file, and mix in a little R, keeping
most of your experiment logic separate. R has two powerful templating packages,
[brew](http://cran.r-project.org/package=brew) and [knitr](http://yihui.name/knitr/).

Brew is almost self-explanatory. Here's a brew file that would recreate the HTML
form above:

```html
<%
name <- if ('name' %in% names(params)) params$name else ''
age <-  if ('age' %in% names(params)) params$age else ''
%>
<html>
<body>
<% if (length(errors) > 0) { %>
  <div style='color:red; border: 1px solid red;'>
  <%= paste(errors, collapse="<br />") %>
  </div>
<% } %>
<form action='' method='POST'>
<h1>Period <%= period %>: enter your details</h1>
<p>Name: <input type='text' name='name' value='<%= name %>'></p>
<p>Age: <input type='number' name='age' value='<%= age %>'></p>
</form>
</body>
</html>
```

Code between `<% %>` tags is evaluated. Code between `<%= %>` tags is 
evaluated and printed out. Notice that the values of `id`, `period`, `params` and 
`errors` are available within the brew file.

To use this within your form stage, use `b_brew`:


```splus
fs <- form_stage(page=b_brew("path_to_brew.html"), fields=list(
        name=has_value(), 
        age=is_between(18,110)
      ), data_frame="mydf")
```


knitr is a similar templating framework. Its syntax is slightly more complex, 
but it has a powerful advantage: it can dynamically generate graphics.

Here's a knitr HTML file that might be part of a public goods game.

```
<!--begin.rcode results='hide'

myid <- which(mydf$id==id & mydf$period==period)
mygroup <- mydf$group[myid]
contribs <- mydf[mydf$group==mygroup, c("contrib", "period", "id")]
contribs <- contribs[order(contribs$period, contribs$id),]
last_contribs <- tail(contribs$contrib, groupsize)

end.rcode-->
<html>
<body>
<h1>Period <!--rinline I(period) --></h1>

<p>Group contributions were:</p>
<!--begin.rcode 
cat(paste(last_contribs, collapse="<br />"))
end.rcode-->. 

<p>History of contributions:</p>

<!--begin.rcode
plot(contribs$period, contribs$contrib, col=contribs$id, type="n",
    xlab="Period", ylab="Contribution", ylim=c(0,50))
for (mem_id in unique(contribs$id)) {
  ct <- contribs[contribs$id==mem_id,]
  lines(ct$period, ct$contrib, col=mem_id, type="b")
}
end.rcode-->

</body>
</html>

```

This prints out an HTML page with an embedded image something like this:

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35.pdf) 


The equivalent of `<% ... %>` in knitr is `<!--begin.rcode ... end.rcode-->`.
You can also use `<!--rinline ... -->` for small chunks of code. knitr prints
images directly in the HTML page, so you don't need to worry about where to put
separate image files. Note that unlike brew, you have to actually print out
the results you want to see using e.g. `cat`. However, `rinline` chunks are
printed out verbatim. A helpful trick which we used above 
is to wrap items in `<!--rinline ... -->` blocks in the function `I()`. 
This prevents knitr surrounding them with special formatting.

betr customizes knitr's options to produce suitable output. See `?b_knit` and 
the [knitr documentation](http://yihui.name/knitr/) for more details. 

Debugging experiments
---------------------

Code can go wrong. Sooner or later, your experiment will throw an error or, 
worse still, will do something unexpected without printing an error message.
Luckily, R has powerful facilities for debugging.

Here's part of a dictator game experiment:


```splus
init_df <- function() {
  mydf <- experiment_data_frame(expt)
  mydf$give <- NA
}
expt <- experiment(N=2, autostart=TRUE, clients_in_url=TRUE, on_ready=init_df)
dict_form <- c(header(), 
      "Choose how much to give: <form method='POST' action=''>",
      "<input type='text' name='give' maxlength='2'>", 
      "<input type='submit' value='Submit'></form>",footer())
dict_stage <- form_stage(dict_form, fields=list(give=is_between(0, 10)), 
      data_frame="mydf")
add_stage(expt, period(), dict_stage)
```


If you run this experiment, you'll get an error:


```splus
ready(expt)
```

```
## starting httpd help server ... done
```

```
## 
## Server started on host 127.0.0.1 and port 35538 . App urls are:
## 
## 	http://127.0.0.1:35538/custom/betr
```

```splus
## web_test(expt) # and submit a number in your browser
```

```
## [1] "<html><head><title>Experiment</title><meta http-equiv='refresh' content='5'></head>\n        <body style='background-color: #CCCCCC; padding: 2% 4%;'>\n        <div style='background-color: white; padding: 3% 3%; \n        border: 1px solid #888888; border-radius: 10px;'>Waiting to start</div><div align='center' style='padding: 10px 10px;'>betr</div></body></html>"
```



This tells us what is wrong: `mydf` has not been created in the global 
environment. At this point, you should look at the code in `init_df`, which
is supposed to create `mydf` when the experiment is made ready. If you are lucky,
you'll notice that it is using `<-` instead of `<<-`: `mydf` is being created
only within the function, not in the global environment. Let's suppose you are
unlucky and don't know why `mydf` isn't being created. We can check what's
happening by using the built-in debugger.


```splus
debug(init_df)
init_df()
```


The call to `debug` means that when `init_df()` is called, you step into it and
evaluate it one line at a time. In the debugger, enter 'n' to move on a line. 
You can also
enter any other R expression and it will be evaluated. So, enter 'n' until you
have run `experiment_data_frame`, then enter `mydf`. You'll see that your data
frame exists and looks OK. Then when you exit the debugger, enter `mydf` again
-- you will see that `mydf` no longer exists. This will hopefully give you a 
clue to what is happening: `mydf` is being created OK, but assigned in the wrong
place. When you've finished debugging, call `undebug(init_df)` to return
to normal behaviour.

Debugging works while you are running an experiment: the browser will hang,
waiting for a response, while you step through the debugger.

Often you will want to debug code that is happening inside an experiment stage.
To do that, call `trace_stage(expt, n, browser)` on your experiment. Here `n` is
the number of the stage in the experiment. This
will put you into debug mode whenever this stage is called. For example, in the
experiment above `trace_stage(expt, 2, browser)` will debug `dict_stage`. Call
`untrace_stage(expt, 2)` to go back to normal mode.

See `?debug`, `?trace`, `?browser` and `?recover` for more details, as well as
the method `trace` in `?setRefClass`.

`replay` is often useful for debugging: see the next section.


Testing experiments
-------------------

We've already seen how to test your experiments manually, using 
`web_test(expt)`. However, if your experiment has 16 participants, this quickly
becomes laborious. Luckily, we can automate the process using `replay`.

In the experiment above, we might want to test several things, such as:

1. Does the experiment start and run without throwing an R error?
2. If subjects get the same number, do they get a profit of $5?
3. If subjects get a different number, do they get a profit of $0?
4. If a subject enters a number less than 0 or greater than 10 or not a whole
number, does the script print an error and ask for input again?

#### Testing with `replay`

To use `replay` you will first need to test your session manually, using 
`web_test`. This may be slow, but you need only do it once. Just by doing this,
you already test question 1 above: you can't run the experiment if it won't run!
When you have finished, check that things are how you expected. Then, note down 
the session name (printed by `info(expt)`). You should find a folder with the 
corresponding session name on your hard drive, in the folder where R started. 
Note the folder name.

Now, make some changes to your code. After you've saved the experiment file and
(perhaps) restarted R, source the file again and enter:


```splus
ready(expt)
replay(expt, folder="[the folder name you noted earlier]")
```


This will replay your previous session _instantly_. Afterwards, you can check again
that things are still as you expect. You don't have to include the folder name:
if you don't, betr will try to find the most recent folder that looks like a
session record.

You can automate the process still further by a testing package such as `testthat`. Here's a simple example, checking that a data frame has the right number of rows
and that the `give` field is always defined.


```splus
test_that("Data frame created OK", {
  source("my-experiment-file.R") # defines an experiment myexp
  replay(myexp, folder="myexp-2014-05-15-120000") # test from 12 pm on 15 May 
  expect_that(nrow(mydf), equals(32)) 
  expect_false(is.na(mydf$give)) # 
})
```


Automated testing makes it easier to run tests -- which should make your
experiments more reliable.
 


Running your experiment in the lab
----------------------------------

When you've tested on your computer, you'll want to try it in the laboratory.
This section describes how to prepare the laboratory to run betr experiments.

### Installing betr on a server computer

The install process for betr is as described above. You could either use a 
computer in the lab, or a remote server. The lab client
computers must be able to connect to this server. 

So far, we've only been serving betr to our own computer. This has hidden a 
small problem: betr relies on the R package 
[Rook](http://cran.r-project.org/package=Rook), and Rook's web server can
_only_ serve to our own computer. Obviously that is not very useful now it is 
time to go live!

The simplest solution to this is to use a proxy web server in front of betr on the
same computer, which redirects requests to betr. betr then serves the requests 
(now coming from its own computer) and the proxy returns them. 

One way to do this is using the standard [Apache](http://httpd.apache.org) web 
server. The steps to do this are:

1. Download and install Apache

On Debian or Ubuntu Linux systems, run `sudo apt-get install apache2`.

2. Enable the Apache module `mod_proxy`. Again, on Debian/Ubuntu:

```
sudo a2enmod mod_proxy
sudo a2enmod proxy_http
sudo service apache2 restart
```

3. Edit your Apache config file to include the line:

```
ProxyPass /betr/ http://127.0.0.1:35538/custom/
```

Here, `35538` is the port betr will serve on internally. (This is the default
port: you can change it using the `port` argument to `experiment()`.)
You can check this works by running something like this in R:


```splus
experiment(N=1, name="foo", autostart=TRUE)
add_stage(experiment, text_stage(c(header(), "Success!", footer())))
ready(expt)
```


Now, if you request the web page `http://your.server.name/betr/foo`, Apache will 
forward this to `http://127.0.0.1:35538/custom/foo`. 

### Kiosk mode

Your lab client computers don't need any special software -- just a web browser.
However, you don't want your subjects to start reading Facebook if they get 
bored. To lock down the web browser, you may wish to install a "Kiosk mode"
extension. This should:

* hide the URL bar and other toolbars
* prevent the user using the back or refresh buttons

Ideally, you also want to prevent users using `Alt+F4` or `Ctrl+Alt+Delete` to
switch applications... but your lab does that already, right? ;-)

Google Chrome comes with a built-in kiosk mode, which you can enable by running
`chrome --kiosk "http://starting.url"`. For Firefox, there is the 
[Rkiosk extension](https://addons.mozilla.org/en-us/firefox/addon/r-kiosk/).
Internet Explorer appears to have a built-in kiosk mode enabled by 
`iexplore.exe -k`.

Another thing you may wish to do is ensure that only certain computers can
connect to your experiment. You can do this with the `auth` argument to 
`experiment`. Here's how to allow only IP addresses starting with
\code{123.121.123}:


```splus
expt <- experiment(N=1, auth=c("123.121.123.*", "127.0.0.1"))
```

```
## Warning: cannot open file 'betr-SEATS.txt': No such file or directory
## Warning: Problem reading seats file betr-SEATS.txt
```


If you need more complex authentication, then you can pass a function to 
`auth`: see `?experiment` for more information.

### Seats and client IDs

In the lab, you need to know how much to pay your subjects. This is typically 
done by seat number. To identify subject seats, betr looks for a _seats file_, 
with the standard name `betr_SEATS.txt`, in its working directory.

The seats file is a tab separated data file which looks like this:

seat | IP | cookie
-----|----|-------
1 | 111.1.1.123 | AFDJKLRE
2 | 111.1.1.124 | REAJKJKL

betr identifies seats either by IP address, or by a cookie set on a client 
machine. (The latter is useful if your lab machines do not have static IP
addresses.)

You can create the seats file by hand, but it easier to use the function 
`identify_seats()`. This starts a simple web application on your server and
tells you the web address it is serving on. You can then go to this address
on each of your lab client computers, and enter the computer's seat number. 
The seats file will be created automatically.

When you call `experiment()`, betr looks for the seats file and prints a warning
if it can't be found. You can pass a non-standard path to the `seats_file` 
argument, or `seats_file=NULL` to suppress this warning and not look for seats
(e.g. in an internet experiment). If the seats file is found, `info(expt)` will
print out clients' seats along with their ID.

At payment time, you can match your data to seats by running `merge_subjects`:


```splus
mydf <<- merge_subjects(expt, mydf)
```


This assumes that `mydf` has a column named `id`. It will add columns including
`seat` to the data. You can then print a list of payments by doing something
like:


```splus
profits <- mydf$[mydf$period=10, c("profit", "seat")]
profits <- profits[order(profits$seat),]
write.csv(profits, file="profits.csv")
```


### Easy commands

You may not always want to run an experiment yourself. If your
experiment administrator does not know R, he or she may find the R command line quite
intimidating. To simplify, you can add the line `load_commands(expt)` to your
source file. Then, instead of typing e.g. `ready(expt)`, the administrator can
just type `READY`:



```splus
myexpt <- experiment(N=3, seats_file=NULL, record=FALSE)
info(expt)
```

```
## Name: betr	Status: Stopped	Clients: 0/1	Periods: 0	Stages: 0
```

```splus
load_commands(myexpt)
READY
```

```
## starting httpd help server ... done
```

```
## 
## Server started on host 127.0.0.1 and port 35538 . App urls are:
## 
## 	http://127.0.0.1:35538/custom/betr
```

```splus
INFO
```

```
## Session: betr-2014-05-20-232617	Status: Waiting	Clients: 0/3	Periods: 0	Stages: 0
## Serving at http://127.0.0.1:35538/custom/betr
```


The commands that can be used are `READY`, `START`, `PAUSE`, `RESTART`, `HALT`, 
`INFO`, `MAP` and ``WEB_TEST`. 

Running experiments online
--------------------------

TODO...
