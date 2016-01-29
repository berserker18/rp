complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'id' is an integer vector containing the monitor id numbers
  ## to be used
  
  ## return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor id number and 'nobs' is the
  ## number of complete cases
  cases <- data.frame()
  for (i in id) {
    d <- read.csv(paste(directory, '/', sprintf('%03d', i), '.csv', sep = ''))
    cases <- rbind(cases, c(i, length(complete.cases(na.omit(d)))))
  }
  names(cases) <- c('id', 'nobs')
  cases
}
