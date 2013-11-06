betr
====

Behavioral Experiments Toolkit in R
-----------------------------------

In your source file:

	myexp <- experiment(port=1234, N=1)
	s1 <- stage(main="<html><body>
		<h1>Welcome to the experiment!</h1>
		</body></html>")
	s2 <- stage(main=function(part, params) {
			pick <- params$pick
			if (is.null(pick)) pick <- ''
			paste0("<html><body><p>Pick a number</p>
			<form action='", url(), "'>
			<input type='text' name='pick' value='", picked, "'>
			</form></body></html>")
		},
		check=function(part, params) {
			if (! is.numeric(params$pick)) stop("Please pick a number!")
			if (params$pick != 7) stop("Guess again!")
		})
	s3 <- stage(main="<html><body><h1>Congratulations!</h1></body></html>")
	add_stage(myexp, s1)
	add_stage(myexp, s2)
	add_stage(myexp, s3)
	
On the command line:
	source("my-experiment.R")
	start(myexp)
