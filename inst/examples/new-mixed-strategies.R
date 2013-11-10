
N <- 4


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
pms <- rep(list(pm1, pm2), each=10)

hta <- "style='font-size:14pt; margin: 5px; border: 1px solid black; border-collapse: collapse'"
htmlhead <- "<head><style type='text/css'>
      td {border: 1px solid black; padding: 15px; font-size: 16pt}
      th {border: 1px solid black; padding: 15px; font-size: 16pt; font-weight: bold;}
      </style></head>"
rc.actions <- list(c("U", "D"), c("L", "R"))
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
          action=NA, matchid=NA, payoff=NA, opp.action=NA))
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
  
  payoff_matrix <<- pms[[(period +1)/2]]
  counter <<- counter + 1 # per subject
  myrc <- if (counter %% 2 ) 1 else 2
  mydf$rc[me_now] <<- myrc
  rc.name <- c("row", "column")[myrc]
  return(paste0("<html>", htmlhead ,"<body style='margin: 70px 70px 70px 70px'>
      <h1>Mixed strategies: period ", (period+1)/2, "</h1>",
      if (nchar(message)>0) paste0("<p style='color:red'>", message, "</p>"),
      "<p>You are the ", rc.name, " player.</p>
      <p>Payoffs for this game are:", 
      print(xtable(payoff_matrix), print.results=FALSE, type="html", 
          html.table.attributes=hta),
      "<form enctype='multipart/form-data' action='' method='POST'>
      Choose an action:<br>
      <button type='submit' name='action' value='", rc.actions[[myrc]][1], "'>",
      rc.actions[[myrc]][1], "</button>
      <button type='submit' name='action' value='", rc.actions[[myrc]][2], "'>", 
      rc.actions[[myrc]][2], "</button></form></body></html>"))
}

s2 <- function(id, period, params) {
  mysubj <- mydf[mydf$period == period - 1,]
  if (nrow(mysubj) < N ||any(is.na(mysubj$action))) return(WAIT)
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
    mydf$opp.action[ mydf$id == mysubj$id[rs][i] & 
        mydf$period==period-1 ] <- cactions[i]
    mydf$payoff[ mydf$id == mysubj$id[cs][i] & 
          mydf$period==period-1 ] <- payoffs[2]
    mydf$opp.action[ mydf$id == mysubj$id[cs][i] & 
        mydf$period==period-1 ] <- ractions[i]
  }
  me <- mydf[mydf$id==id & mydf$period==period-1,]
  html <- paste0("<html>", htmlhead ,
                "<body style='margin: 70px 70px 70px 70px'><h1>
                Results for period ", period/2, "</h1>
                <p>Payoffs were:</p>", print(xtable(payoff_matrix), type="html",
                print.results=FALSE, html.table.attributes=hta))
  if (is.na(me$payoff)) {
    html <- paste0(html, "<p>You were not matched in this round</p>")
  } else {
    html <- paste0(html, "<p>You played ", rc.actions[[me$rc]][me$action],
      ". You were matched with an opponent who played ", 
      rc.actions[[3 - me$rc]][me$opp.action],
      ". You scored ", me$payoff , " points.</p>")
  }
  
  rpctU <- 100 * sum(mysubj$rc==1 & mysubj$action==1, na.rm=T) /
        sum(mysubj$rc==1)
  cpctL <- 100 * sum(mysubj$rc==2 & mysubj$action==1, na.rm=T) /
        sum(mysubj$rc==2)
  html <- paste0(html, "<br><br><p>Row players: ", rpctU, 
        "% played U, ", 100-rpctU, "% played D</p>")
  html <- paste0(html, "<br><br><p>Column players: ", cpctL, 
        "% played L, ", 100-cpctL, "% played R</p>")
  html <- paste0(html, "<form action='' method='post'>
          <input type='submit' name='moveon' value='Next game'></form>
          </body></html>")
  return(html)
}

expt <- experiment(auth=TRUE, server="RookServer", N=N, autostart=TRUE,
      clients_in_url=TRUE, name="mix")
add_stage(expt, s1, s2, times=10) 
ready(expt)
