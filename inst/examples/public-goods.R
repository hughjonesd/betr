
library(betr)

expt <- experiment(N=N, clients_in_url=TRUE, name="public-goods")
with(environment(expt), {
  brewdir <- file.path(find.package("betr"), "examples")
  
  N <- 16
  mydf <- data.frame(id=numeric(0), group=numeric(0), contrib=numeric(0), 
    timed_out=logical(0))
  gsample <- matrix(sample(1:N), nrow=4)
  groups <- lapply(1:ncol(gsample), function(x) gsample[,x])  
})

s1 <- structured_stage(
  form = file(file.path(brewdir, "public-goods-form.brew")),
  process = function (id, period, params) {
    if (! params$contrib %in% 0:10) stop("Please pick a contribution between 0 and 10")
    mydf$contrib[mydf$id==id & mydf$period==period] <<- params$contrib
    mydf$timed_out[mydf$id==id & mydf$period==period] <<- FALSE
  },
  timeout = 120,
  on_timeout = function(id, period) {
    mydf$contrib[mydf$id==id & mydf$period==period] <<- params$contrib
    mydf$timed_out[mydf$id==id & mydf$period==period] <<- TRUE
  },
  wait_for = groups,
  result = function (id, period, params) {
    me <- mydf$id==id & mydf$period==period
    mygroup <- mydf$group==mydf$group[me] & mydf$period==period
    mydf$group.contrib[me] <<- sum(mydf$contrib[mygroup])
    mydf$earnings[me] <<- 10 - mydf$contrib[me] + mydf$group.contrib[me] * 
          multiplier / 4
    if (mydf$timed_out[me]) mydf$earnings[me] <<- 0
    capture.output(brew(file.path(brewdir, "public-goods-result.brew")))
  }
)

add_stage(expt, s1, times=10)

cat("Call ready(expt) to begin waiting for clients.", file=stderr())
  
