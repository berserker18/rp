best <- function(state, outcome) {
  ## read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')

  ## check that state and outcome are valid
  state_data <- subset(data, State == state)
  if (nrow(state_data) == 0) {
    stop('invalid state')
  }

  ## ref: names(data)
  if (outcome == 'heart attack') {
    d <- 11
  } else if (outcome == 'heart failure') {
    d <- 17
  } else if (outcome == 'pneumonia') {
    d <- 23
  } else {
    stop('invalid outcome')
  }

  ## return hospital name in that state with lowest 30-day death rate
  state_outcome_data <- na.omit(state_data[,c(2,d)])
  state_outcome_data[,2] <- as.numeric(state_outcome_data[,2])
  state_outcome_data[order(state_outcome_data[,2], state_outcome_data[,1]),1][1]
}