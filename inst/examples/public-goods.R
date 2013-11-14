
library(betr)
library(brew)

# parameters
N <- 4
periods <- 10
multiplier <- 1.6
partner <- FALSE # FALSE for stranger design, TRUE for partner
groupsize <- 4

brewdir <- system.file("examples", package="betr")
Nt <- N * periods
expt <- experiment(N=N, clients_in_url=TRUE, name="public-goods", autostart=TRUE)
with(environment(expt), {
  mydf <- data.frame(id=rep(1:N, periods), period=rep(1:periods, each=N), 
        group=NA_integer_, contrib=NA_integer_, timed_out=NA)
  for (i in 1:periods) mydf$group[mydf$period==i] <- sample(rep(1:(N/groupsize),
        groupsize))
  if (partner && i > 1) mydf$group[mydf$period==i] <- mydf$group[mydf$period==1]
})


s1 <- structured_stage(
  form = file(file.path(brewdir, "public-goods-form.brew")),
  
  process = function (id, period, params) {
    me <- mydf$id==id & mydf$period==period
    if (! "contrib" %in% names(params)) stop("Please pick a contribution")
    contrib <- as.numeric(params$contrib)  
    if (! contrib %in% 0:10) stop("Please pick a contribution between 0 and 10")
    mydf$contrib[me] <<- contrib
    mydf$timed_out[me] <<- FALSE
  },
  
  timeout = 120,
  
  on_timeout = function(id, period) {
    me <- mydf$id==id & mydf$period==period
    mydf$contrib[me] <<- 0
    mydf$timed_out[me] <<- TRUE
  },
  
  wait_for = function (...) {
    mygroup <- mydf$group[mydf$id==id & mydf$period==period]
    all(mydf$id[mydf$group==mygroup & mydf$period==period] 
        %in% ready)
  },
  
  result = function (id, period, params) {
    me <- mydf$id==id & mydf$period==period
    mygroup <- mydf$group==mydf$group[me] & mydf$period==period
    mydf$group.contrib[me] <<- sum(mydf$contrib[mygroup])
    mydf$earnings[me] <<- 10 - mydf$contrib[me] + mydf$group.contrib[me] * 
          multiplier / groupsize
    if (mydf$timed_out[me]) mydf$earnings[me] <<- 0
    capture.output(brew(file.path(brewdir, "public-goods-result.brew")))
  }
)

add_stage(expt, s1, times=periods)

cat("Call ready(expt) to begin waiting for clients.", file=stderr())
  
