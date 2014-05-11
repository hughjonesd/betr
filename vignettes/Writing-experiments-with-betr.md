
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

```{splus eval=FALSE}
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


Installing betr
---------------

To install betr just run:

```{splus eval=FALSE}
install.packages("devtools") # if not already installed
library(devtools)
install_github("betr", "hughjonesd") # for the latest version
```

An example betr experiment
--------------------------

Typically you will define your experiment in a source file. Then, during a 
session, you will run it from the command line. Here's a source file for a
simple guessing game experiment.

```{splus warning=FALSE}
library(betr)
expt <- experiment(N=1, clients_in_url=TRUE)

s1 <- function(id, period, params) {

  me_now <- mydf$id==id & mydf$period==period
  gg <- ! missing(params) && 'guess' %in% names(params) 
  if (gg) {
    mydf$guess[me_now] <<- params$guess
    mydf$correct[me_now] <<- if (params$guess == sample(1:10,1)) 1 else 0
    return(NEXT)
  }
  last_guess <- ''
  me_before <- mydf$id==id & mydf$period == period -1
  if (period > 1) last_guess <- paste('You guessed ', mydf$guess[me_before], 
        '... you were ', if(mydf$correct[me_before]>0) 'right!' else 'wrong!', sep='')
  return(paste(header(), sprintf(
    "<p color='red'>%s</p>
    <p>Pick a number</p>
    <form action='' method='post'><select name='guess'>%s</select>
    <input type='submit' value='Submit'></form>",
    last_guess,
    paste("<option value='", 1:10,"'>", 1:10, "</option>", sep="", collapse=""
  )), footer()))
}

add_stage(expt, period(), s1, times=5)


initialize_data_frame <- function() {
  mydf <<- experiment_data_frame(expt)
  mydf$guess <<- NA
  mydf$correct <<- NA
}
on_ready(expt, initialize_data_frame)
```

Let's look at this bit by bit.
```{splus warning=FALSE}
expt <- experiment(N=1, clients_in_url=TRUE)
```
The call to `experiment` returns an object of class Experiment. `N=1` gives the
number of participants. We'll come to the other options in a bit.

Next, we define a function called `s1`. In betr, experiments are composed of
_stages_. `s1` is a simple stage. Ignoring the details, the structure of `s1`
is like:

```{splus warning=FALSE}
s1 <- function(id, period, params) {
  # ...
  return(NEXT)
  # or ...
  return("some HTML")
}
```

Here's how this works. When a subject makes an HTTP request to betr, the current
stage is called with the subject's `id` (usually randomly generated), the
subject's current `period`, and any parameters from the request -- e.g.
data from a form filled in by the subject.

The stage can then do one of three things:

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


```{splus warning=FALSE}
add_stage(expt, period(), s1, times=5)
```

To create our experiment we need to add `s1` to it. What about the other thing 
here, `period()`?
In fact, `period()` creates another kind of stage, called (guess what?) a 
Period. This stage is very simple: it simply increments the period counter by 1,
then returns `NEXT`. 

Lastly, the `times=5` argument repeats the whole sequence of stages 5 times. So
our experiment's stages are now:

> period, s1, period, s1, period, s1, period, s1, period, s1

You should usually start your experiment with a Period to set the period counter 
to 1.

```{splus}
initialize_data_frame <- function() {
  mydf <<- experiment_data_frame(expt)
  mydf$guess <<- NA
  mydf$correct <<- NA
}
on_ready(expt, initialize_data_frame)
```

Last of all, we want to prepare a data frame for our experiment. We could just 
do this by calling

```{splus}
mydf <<- experiment_data_frame(expt)
```

This would create a new data frame with 5 * 1 = 5 rows -- 5 for the number of
periods, 1 for the number of subjects -- and with columns `id` and `period`. 
(There's no magic about `experiment_data_frame`: you could just do 
`mydf <- data.frame(id=1, period=1:5)`.)

However, we want to make sure that our experiment is _replay-safe_. If we 
replay our experiment, either during testing or to recover from a problem in a
session, our experiment will be rerun from the start. But if our data has 
already been written, then this might affect how things work. To make sure
that replay will work exactly the same, we create a function 
`initialize_data_frame` that creates a fresh empty dataset. We then pass
this to the experiment using `on_ready(expt, initialize_data_frame)`. Our
function will be called whenever the experiment is made ready, including
when it is replayed. Then the replay will populate our data frame just as it was.

OK, we've created our experiment. Now, we need to run it. Run the code above, our
put it in a file called `my_experiment.R` and source it: 
```{splus eval=FALSE}
source("my_experiment.R")
```
You'll see some warnings about "seats" -- don't worry about those for now.

Before we run it, let's take a look at it. On the command line, type
```{splus}
expt
```
You should see a line like

> Name: betr  Status: Stopped	Clients: 0/1	Periods: 5	Stages: 10

This gives you basic information about the experiment. _Clients_ tells you how
many clients have connected, out of the experiment's N. _Periods_ and 
_Stages_ are self-explanatory. There are 5 periods because our experiment
had 5 periods in its stages. There are 10 stages including the 5 periods and
the 5 `s1` objects. Lastly, the _Status_ tells us whether the experiment is 
_Stopped_, _Waiting_, _Started_ or _Paused_. Right now it is _Stopped_. Let's
change that. On the command line, run:

```{splus error=FALSE}
ready(expt)
```

Calling `ready` does several things:
* Starts the web server. Now, subject computers can connect to the experiment;
* Creates a new experiment session, with a date and time, and creates a folder
on disk to hold data about it.
* Calls any function passed to `on_ready`, e.g. initializing your data.

Now, if you enter `expt` again, you should see something like:

> Session: betr-2014-05-11-130421  Status: Waiting	Clients: 0/1	Periods: 5	Stages: 10
> Serving at http://127.0.0.1:10946/custom/betr

The status has changed, and we see the session name. We also have a URL. Clients
can connect to this to view the experiment. You can do this manually: open your 
web browser and go to the URL _http://127.0.0.1:10946/custom/betr/client-1_. Or,
for a convenient shortcut, enter:
```{splus eval=FALSE}
web_test(expt)
```
on the R command line. You should see a page saying "Waiting to start". It will 
refresh regularly. 

Before we start, let's see a bit more information about our experiment. Type:

```{splus}
info(expt)
```

This shows the same info as before, plus a list of subjects -- just one.

To start the experiment, type:

```{splus eval=FALSE}
start(expt)
```

Now, when your browser page refreshes, you will see the first period of the 
guessing game. Complete a couple of guesses (good luck!). Use `info(expt)`
to watch your subject progressing. You can also look directly at your 
experimental data frame:
```{splus eval=FALSE}
mydf
```
>  id period guess correct
>1  1      1     1       0
>2  1      2     6       0
>3  1      3     1       0
>4  1      4  <NA>      NA
>5  1      5  <NA>      NA

At the end, you will see an "experiment finished" page in your browser, and
`info(expt)` will show that your subject has finished the experiment.

To stop the experiment serving, run

```{splus}
halt(expt)
```

Now the server will halt, so clients can no longer connect, and the status
of the experiment will be "Stopped". 

Lastly, let's replay our experiment. Enter

```{splus eval=FALSE}
replay(expt, ask=TRUE)
```

The command prompt should show 

> replay>

Enter 'd' to see details of the first request from a client computer. Then enter
'n' to replay this request. Carry on entering 'd' and 'n'; eventually you will 
see your 'start' command. Enter 'n' three more times. Now enter `mydf`. You
will see that your data frame is back as it was in the middle of the experiment.
You can enter arbitrary R expressions. For a list of other commands, enter 
'h' or '?'.

Writing experiments
-------------------

Testing experiments
-------------------

Debugging
---------

Running your experiment in the lab
----------------------------------

### SEATS

### Apache/Rapache/Rook