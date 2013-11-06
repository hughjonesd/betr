
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
expt <- experiment(auth=TRUE, server="RookServer", N=2)
s1 <- stage(
  setup = function() counter <<- 0,
  main=function() {
    payoff_matrix <- pms[[(period+1)/2]]
    counter <<- counter + 1
    rc <- if (counter %% 2 ) 1 else 2
    rc.actions <- list(c("U", "D"), c("L", "R"))[rc]
    rc.name <- c("row", "column")[rc]
      paste0("<html><body>
      <h1>Mixed strategies</h1>
      <p>You are the ", rc.name, " player.</p>
      <p>Payoffs for this game are:", 
      print(xtable(payoff_matrix), type="html"),
      "<form action=''>Choose an action:<br>
      <input type='submit' name='action' value='", rc.actions[1], "'>
      <input type='submit' name='action' value='", rc.actions[2], "'> 
      </form></body></html>")
  },
  check = function(part, params) {
    if ('action' %in% names(params)) {
      cur_subj <- subjects$period==period & subjects$id == part$id
      subjects$action[cur_subj] <- params$action
      subjects$rc[cur_subj] <- ifelse(params$action %in% c("U", "D"), 1, 2)
    } else {
      stop("Please choose an action")
    }
  },
  finish = function(period) {
    rs <- subjects$period==period & subjects$rc==1
    cs <- subjects$period==period & subjects$rc==2
    prs <- min(sum(rs),sum(cs))
    rand_order <- sample(1:cs, prs)
    ractions <- subjects$action[rs][1:prs]
    cactions <- subjects$action[cs][rand_order]
    payoff_matrix <- pms[[period]]
    subjects$points[subjects$period==period] <<- NA
    for (i in 1:prs) {
      payoffs <- payoff_matrix[ ractions[i], cactions[i] ]
      subjects$points[rs][i] <<- payoffs[1]
      subjects$points[rand_order][i] <<- payoffs[2]
    }
  },
  wait_for="none"
)
s2 <- stage(
  wait_for="all",
  main=function() {
    html <- paste0("<html><body><h1>Results</h1>
          <p>Payoffs were:</p>", print(xtable(payoff_matrix)))
    if (is.na(subjects$points[subjects$period==period & subjects$id==cur_id])) {
      html <- paste0(html, "<p>You were not matched in this round</p>")
    } else {
      html <- paste0(html, "<p>You played ", subjects[] , 
            " and received ", , " points</p>")
    }
    html <- paste0(html, "<form action=''>
          <input type='submit' value='Next game'></form>
          </body></html>")
    return(html)
  }
)
add_stage(expt, s1)