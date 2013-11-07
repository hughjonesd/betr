

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

pm1 <- p_m(1,0,0,1,0,1,1,0)
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
mysubjects <- data.frame(id=numeric(), action=numeric(), rc=numeric(), 
      period=numeric(), matchid=numeric(), payoff=numeric())

update_subjects <- function(..., id=id, period=period) {
  suppressWarnings(mysubjects[mysubjects$id==id & mysubjects$period==period,] <<- 
        within(mysubjects[mysubjects$id==id & mysubjects$period==period], ...))
}

s1 <- function(id, period, params) {
  id <- id
warning("Period is: ", period)
  message <- ""
  if (nrow(subset(mysubjects, period==period & id==id)) == 0) {
    mysubjects <<- rbind(mysubjects, data.frame(id=id, period=period, rc=NA, 
          action=NA, matchid=NA, payoff=NA))
  }
  me <- subset(mysubjects, id==id & period==period) 
  
  if ("action" %in% names(params)) {
    if (params$action %in% rc.actions[[me$rc]]) {
      update_subjects(action <- if (params$action %in% c("U", "L")) 1 else 2, 
            id=id, period=period)
      return(NEXT)
    } else {
      message <- "Please choose one of the two actions!"
      warning("Got action not in 1 or 2 from id ", me$id, "; action was: ",
            params$action, " rc.actions[[me$rc]] was: ", rc.actions[[me$rc]])
      str(me)
    }
  }
  
  mcount <<- mcount + 1
  payoff_matrix <<- pms[[mcount]]
  counter <<- counter + 1 
  myrc <- if (counter %% 2 ) 1 else 2
  update_subjects(rc <- myrc, id=id, period=period)
  rc.name <- c("row", "column")[myrc]
  return(paste0("<html><body>
      <h1>Mixed strategies</h1>",
      if (nchar(message)>0) paste0("<p style='color:red'>", message, "</p>"),
      "<p>You are the ", rc.name, " player.</p>
      <p>Payoffs for this game are:", 
    print(xtable(payoff_matrix), print.results=FALSE, type="html"),
    "<form action='' method='POST'>Choose an action:<br>
      <input type='submit' name='action' value='", rc.actions[[myrc]][1], "'>
      <input type='submit' name='action' value='", rc.actions[[myrc]][2], "'> 
      </form></body></html>"))
}

s2 <- function(subject, period, params) {
  mysubj <- mysubjects[mysubjects$period == period - 1,]
  if (any(is.na(mysubj$action))) return(WAIT)
  
  mysubj <- mysubj[order(sample(nrow(mysubj))),]
  rs <- mysubj$rc == 1
  cs <- mysubj$rc == 2
  prs <- min(sum(rs),sum(cs))
  ractions <- mysubj$action[rs][1:prs]
  cactions <- mysubj$action[cs][1:prs]
  points <- rep(NA, nrow(mysubj))
  for (i in 1:prs) {
    payoffs <- payoff_matrix[ ractions[i], cactions[i] ]
    update_subjects(payoff <- payoffs[1], id = mysubj$id[rs][i], period=period - 1)
    update_subjects(payoff <- payoffs[2], id = mysubj$id[cs][i], period=period - 1)
  }
  
  me <- subset(mysubjects, id==id & period==period-1)
  html <- paste0("<html><body><h1>Results</h1>
          <p>Payoffs were:</p>", print(xtable(payoff_matrix)))
  if (is.na(me$payoff)) {
    html <- paste0(html, "<p>You were not matched in this round</p>")
  } else {
    
    html <- paste0(html, "<p>You played ", rc.actions[me$action], 
      " and received ", , " points</p>")
  }
  
  rpctU <- 100 * sum(mysubj$rc==1 & mysubj$action=="U")/sum(mysubj$rc==1)
  cpctL <- 100 * sum(mysubj$rc==2 & mysubj$action=="L")/sum(mysubj$rc==2)
  html <- paste0(html, "<br><br><p>Row players: ", rpctU, 
        "% played U, the rest played D</p>")
  html <- paste0(html, "<br><br><p>Column players: ", cpctL, 
        "played L, the rest played R</p>")
  html <- paste0(html, "<form action='", this_url ,"'>
          <input type='submit' value='Next game'></form>
          </body></html>")
  return(html)
}

expt <- experiment(auth=TRUE, server="RookServer", N=1)
add_stage(expt, s1, s2, reps=10) # each would be s1 x 10, s2 x 10, s3 x 10
ready(expt)
