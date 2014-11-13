

# plan:
# list of fields passed to experiment()
# also optional groupsize, partner etc.
# no guarantee that N is constant over periods (internet experiments)
# subjects() has data for current period, including stage, id, client etc.
# new fields include group, profit
# write_subjects() just calls write.csv
# convenience function display_payments(), print_receipts() etc.?
# group() gets group(s) of subj id, period

create_data <- function(experiment, N=NULL, periods=NULL, partner=TRUE, 
      groupsize=NULL, ...) {
  if (missing(periods) || is.null(periods)) periods <- nperiods(experiment)
  if (missing(N) || is.null(N)) N <- experiment$N
  if (periods < 1) stop("Experiment must have at least one period!")
  arglist <- list(...)
  arglist$period <- rep(1:periods, each=N)
  arglist$id <- rep(1:N, periods)
  if (! missing(groupsize) && ! is.null(groupsize)) {
    if (length(groupsize) == 1 && N %% groupsize > 0) 
          stop("N must be an exact multiple of groupsize")
    if (length(groupsize) > 1 && sum(groupsize) != N) 
          stop("group sizes must add up to N")
    arglist$group <- if (length(groupsize)==1) rep(1:(N/groupsize), groupsize) else 
          rep(1:length(groupsize), groupsize)
    arglist$group <- sample(arglist$group)
  } else {
    arglist$group <- NA_integer_
  }
  arglist$profit <- 0
  arglist$stringsAsFactors <- FALSE
  arglist$period <- 0
  arglist$stage <- 0
  arglist$client <- NA_character_
  subjects <- do.call(data.frame, arglist)
  if (! partner) for (i in 1:periods) subjects$group[subjects$period==i] <- 
        sample(subjects$group[subjects$period==i]) 
  return(subjects)
}


