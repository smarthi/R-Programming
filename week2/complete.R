complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  filenames = list.files(directory, full.names = TRUE)
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  airquality <- read.table(text="", header=TRUE, col.names = c('id', 'nobs'))
  for (i in id) {
    df <- rbind(airquality, c(i, sum(complete.cases(read.csv(filenames[i])))))
  }
  colnames(airquality) <- c("id", "nobs")
  df
}