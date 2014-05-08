betr
====

Behavioral Experiments Toolkit in R
-----------------------------------

Economic-style experiments in R. Principles:

* Make testing and debugging painless
* Use the web browser to handle the network
* Allow easy recovery and rewind if things go wrong

Installation
------------

```{r}
install.packages("devtools") # if not installed already
library(devtools)
install_github("betr", "hughjonesd")
```

Writing experiments
-------------------

A source file for a simple guessing game:

```{r}

mydf <<- data.frame(id=1, period=1:5, guess=NA, correct=NA)
expt <- experiment(N=1, autostart=TRUE)

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
  if (period > 1) last_guess <- paste0('You guessed ', mydf$guess[me_before], 
        '... you were ', if(mydf$correct[me_before]>0) 'right!' else 'wrong!')
  
  return(sprintf(
    "<html><body><p color='red'>%s</p><p>Pick a number</p>
    <form action='' method='post'><select name='guess'>%s</select>
    <input type='submit' value='Submit'></form</body></html>",
    last_guess,
    paste0("<option value='", 1:10,"'>", 1:10, "</option>", collapse="")
  ))
}

add_stage(expt, s1, times=5)

```


Running your experiment
-----------------------

On the command line:

```{r}
> source("my-experiment.R")
> ready(expt)
```

To see experiment info:

```{r}
> info(expt)
```

When participants are at their computers:

```{r}
> start(expt)
```

Developing and testing
----------------------

Add `clients_in_url=TRUE` to your call to `experiment`. Then, on the command line:

```{r}
> source("my-experiment.R")
> ready(expt)
> browser_test(expt) # launches many browser windows!
```

After running a session, you can replay it:

```{r}
> replay(expt) 
```

Or replay step by step and watch how things happen:

```{r}
> replay(expt, ask=TRUE) 
```


Or replay a particular session:

```{r}
> replay(expt, folder="betr-2014-05-29-120000") 
```


	
