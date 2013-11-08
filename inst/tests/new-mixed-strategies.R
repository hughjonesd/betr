

library(xtable)

p_m <- function (...) {
  pm <- array(c(...), dim=c(2,2,2), 
    dimnames=list(row=c("U", "D"),column=c("L", "R"),player=1:2))
  class(pm) <- "PayoffMatrix"
  pm
}
xtable.PayoffMatrix <- function(pm) {
  tbl <- apply(pm, 1:2, function(x) paste(x[1], x[2], sep=","))
  xtable(tbl)
}

pm1 <- p_m(2,0,0,1,0,3,4,0)
pm2 <- p_m(3,1,0,2,0,1,1,0)
pms <- list(
  period1 = pm1,
  period2 = pm1,
  period3 = pm1,
  period4 = pm2,
  period5 = pm2,
  period6 = pm2
)

rc.actions <- list(c("U", "D"), c("L", "R"))
mcount <- 1
counter <- 1
mydf <- data.frame(id=numeric(), action=numeric(), rc=numeric(), 
      period=numeric(), matchid=numeric(), payoff=numeric())

update_subjects <- function(..., id=id, period=period) {
  suppressWarnings(mydf[mydf$id==id & mydf$period==period,] <<- 
        within(mydf[mydf$id==id & mydf$period==period], ...))
}

s1 <- function(id, period, params) {
  id <- id
  message <- ""
  
  if (nrow(mydf[mydf$period==period & mydf$id==id,]) == 0) {
    mydf <<- rbind(mydf, data.frame(id=id, period=period, rc=NA, 
          action=NA, matchid=NA, payoff=NA))
  }
  me_now <- mydf$id==id & mydf$period==period
  
  if (! missing(params) && "action" %in% names(params)) {
    if (params$action %in% rc.actions[[ mydf$rc[me_now] ]]) {
      mydf$action[me_now] <<- if (params$action %in% c("U", "L")) 1 else 2 
      return(NEXT)
    } else {
      message <- "Please choose one of the two actions!"
    }
  }
  
  mcount <<- mcount + 1
  payoff_matrix <<- pms[[mcount]]
  counter <<- counter + 1 
  myrc <- if (counter %% 2 ) 1 else 2
  mydf$rc[me_now] <<- myrc
  rc.name <- c("row", "column")[myrc]
  return(paste0("<html><body>
      <h1>Mixed strategies</h1>",
      if (nchar(message)>0) paste0("<p style='color:red'>", message, "</p>"),
      "<p>You are the ", rc.name, " player.</p>
      <p>Payoffs for this game are:", 
      print(xtable(payoff_matrix), print.results=FALSE, type="html"),
      "<form enctype='multipart/form-data' action='", self_url() ,
      "' method='POST'>Choose an action:<br>
      <button type='submit' name='action' value='", rc.actions[[myrc]][1], "'>",
      <button type='submit' name='action' value='", rc.actions[[myrc]][2], "'>", 
      rc.actions[[myrc]][2], "</button></form></body></html>"))
}

s2 <- function(id, period, params) {
  mysubj <- mydf[mydf$period == period - 1,]
  if (any(is.na(mysubj$action))) return(WAIT)
  if (! missing(params) && 'moveon' %in% names(params)) return(NEXT)
  
  mysubj <- mysubj[order(sample(nrow(mysubj))),]
  rs <- mysubj$rc == 1
  cs <- mysubj$rc == 2
  prs <- min(sum(rs),sum(cs))
  ractions <- mysubj$action[rs][1:prs]
  cactions <- mysubj$action[cs][1:prs]
  points <- rep(NA, nrow(mysubj))
  for (i in 1:prs) {
    payoffs <- payoff_matrix[ ractions[i], cactions[i], ]
    mydf$payoff[ mydf$id == mysubj$id[rs][i] & 
          mydf$period==period-1 ] <- payoffs[1]
    mydf$payoff[ mydf$id == mysubj$id[cs][i] & 
          mydf$period==period-1 ] <- payoffs[2]
  }
  me <- mydf[mydf$id==id & mydf$period==period-1,]
  html <- paste0("<html><body><h1>Results</h1>
          <p>Payoffs were:</p>", print(xtable(payoff_matrix), type="html",
          print.results=FALSE))
  if (is.na(me$payoff)) {
    html <- paste0(html, "<p>You were not matched in this round</p>")
  } else {
    html <- paste0(html, "<p>You played ", rc.actions[me$action], 
      " and received ", me$payoff , " points</p>")
  }
  
  rpctU <- 100 * sum(mysubj$rc==1 & mysubj$action=="U")/sum(mysubj$rc==1)
  cpctL <- 100 * sum(mysubj$rc==2 & mysubj$action=="L")/sum(mysubj$rc==2)
  html <- paste0(html, "<br><br><p>Row players: ", rpctU, 
        "% played U, ", 100-rpctU, "% played D</p>")
  html <- paste0(html, "<br><br><p>Column players: ", cpctL, 
        "% played L,", 100-cpctL, "% played R</p>")
  html <- paste0(html, "<form action='", self_url() ,"' method='post'>
          <input type='submit' name='moveon' value='Next game'></form>
          </body></html>")
  return(html)
}

expt <- experiment(auth=TRUE, server="RookServer", N=2, autostart=TRUE,
      client_param="client")
add_stage(expt, s1, s2, times=4) # each would be s1 x 10, s2 x 10, s3 x 10
ready(expt)
