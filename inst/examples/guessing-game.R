
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
  if (period > 1) last_guess <- paste('You guessed ', mydf$guess[me_before], 
        '... you were ', if(mydf$correct[me_before]>0) 'right!' else 'wrong!', sep='')
  return(sprintf(
    "<html><body>
    <p color='red'>%s</p>
    <p>Pick a number</p>
    <form action='' method='post'><select name='guess'>%s</select>
    <input type='submit' value='Submit'></form</body></html>",
    last_guess,
    paste("<option value='", 1:10,"'>", 1:10, "</option>", sep="", collapse=""
  )))
}
add_stage(expt, s1, times=5)
ready(expt)
