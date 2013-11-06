betr
====

Behavioral Experiments Toolkit in R
-----------------------------------

In your source file:

```R
myexp <- experiment(port=1234, N=1)
s1 <- stage(main="<html><body>
	<h1>Welcome to the experiment!</h1>
	</body></html>")
s2 <- stage(
	before=function(part) {
		part$guesses <- 1
	}
	main=function(part, params) {
		pick <- params$pick
		if (is.null(pick)) pick <- ''
		paste0("<html><body><p>Pick a number</p>
		<form action='", url(), "'>
		<input type='text' name='pick' value='", picked, "'>
		</form></body></html>")
	},
	check=function(part, params) {
		if (! is.numeric(params$pick)) stop("Please pick a number!")
		if (params$pick != 7) {
			part$guesses <- part$guesses + 1
			stop("Guess again!")
		}
	},
	finish=function(period) {
		subjects$won <- subjects$guesses == min(subjects$guesses)
	}
)
s3 <- stage(main="<html><body><h1>Congratulations!</h1></body></html>")
add_stage(myexp, s1)
add_stage(myexp, s2)
add_stage(myexp, s3)
```

On the command line:

```R
> source("my-experiment.R")
> ready(myexp)
```
... and when your participants are at their computers:

```R
> start(myexp)
```


	
