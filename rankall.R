rankall <- function(outcome, num = 'best') {
  ## read outcome data
  ##o <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', colClasses = 'character')
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')

  ## check that state and outcome are valid
  if (outcome == 'heart attack') {
    d <- 11
  } else if (outcome == 'heart failure') {
    d <- 17
  } else if (outcome == 'pneumonia') {
    d <- 23
  } else {
    stop('invalid outcome')
  }

  ## for each state, find the hospital of the given rank
  df <- data.frame()
  i <- 1
  for (state in sort(unique(data$State))) {
    df[i,1] <- rankhospital(state, outcome, num)
    df[i,2] <- state
    i <- i + 1
  }
  #print(df)
  colnames(df) <- c('hospital','state')

  ## return a data frame with the hospital names and the (abbreviated) state name
  df
}