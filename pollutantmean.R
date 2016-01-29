pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"

  ## 'id' is an integer vector containing the monitor id numbers
  ## to be used

  ## return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring na values)
  ## note: do not round the result!
  readings <- numeric()
  for (i in id) {
    d <- read.csv(paste(directory, '/', sprintf('%03d', i), '.csv', sep = ''))
    if (pollutant == 'sulfate') {
      readings <- append(readings, d[,2])
    } else {
      readings <- append(readings, d[,3])
    }
  }
  mean(readings, na.rm = T)
}
