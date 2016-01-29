corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## return a numeric vector of correlations
  ## note: do not round the result!
  correlations <- numeric()
  for (i in 1:332) {
    d <- read.csv(paste(directory, '/', sprintf('%03d', i), '.csv', sep = ''))
    if (length(complete.cases(na.omit(d))) > threshold) {
      correlations <- append(correlations, cor(d[,2], d[,3], use = 'pairwise.complete.obs'))  
    }
  }
  correlations
}
