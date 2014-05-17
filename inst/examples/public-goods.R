
library(betr)

# parameters
N <- 4
periods <- 2
multiplier <- 1.6
partner <- FALSE # FALSE for stranger design, TRUE for partner
groupsize <- 2
max_contrib <- 50
timeout <- 60

initialize <- function() {
  mydf <<- experiment_data_frame(expt, group=NA, contrib=NA, 
        timed_out=FALSE, earnings=NA, final_earnings=NA, payment_period=NA)
  for (i in 1:periods) {
    mydf$group[mydf$period==i] <<- if (i==1 || ! partner) 
      sample(rep(1:(N/groupsize), groupsize)) else mydf$group[mydf$period==1]
  }
}
expt <- experiment(N=N, clients_in_url=TRUE, name="public-goods", 
      autostart=TRUE, on_ready=initialize)
  
payment_period <- NA

myinstructions <- text_stage(text=c(header(), "Instructions go here", footer()), 
      wait=TRUE)

myform <- form_stage(
      form_page=c(header(), "<p>Pick a contribution:</p>",
        "<form action='' method='POST'>", sprintf("<input type='number' 
        name='contrib' min='0' max='%s' step='1' autocomplete='off'/>", 
        max_contrib), "<input type='submit' value='Submit' /></form>", footer()),
    fields=list(contrib=all_of(is_whole_number(), is_between(0, max_contrib)))
  , data_frame="mydf")
  
# myform <- timed(myform, timeout, on_timeout=function(id, period) {
#   mydf$timed_out[mydf$id==id & mydf$period==period] <<- TRUE
# })
  
myprog <- program("first", function(id, period) {
  tmp <- mydf[mydf$period==period,]
  tmp$contrib <- as.numeric(tmp$contrib)
  profit <- ave(tmp$contrib, tmp$group, FUN=function(x) max_contrib - x + 
        multiplier * mean(x))
  profit[tmp$timed_out] <- 0
  mydf$earnings[mydf$period==period] <<- profit
})

finalprog <- program("first", function(id, period) {
  payment_period <- sample(1:periods, 1)
  mydf$payment_period <<- payment_period
  mydf$final_earnings <<- mydf$earnings[mydf$period==payment_period]
  write_data(expt, mydf)
})

sfinal <- text_stage(text=c(header(), sprintf("<p>You earned $%2f</p>", 
      mydf$final_earnings[mydf$id==id & mydf$period==payment_period]), 
      footer()))

add_stage(expt, myinstructions)
add_stage(expt, period(), myform, checkpoint("all"), myprog, times=periods)
add_stage(expt, finalprog, sfinal)
  
