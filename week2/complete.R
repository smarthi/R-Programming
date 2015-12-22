complete <- function(directory, id = 1:332) {
  
  results <- matrix(ncol=2, nrow = 0)
  
  for (i in id) {
    data <- getmonitor(i, directory)
    temp <- data[!is.na(data[,"sulfate"]),]
    temp <- temp[!is.na(temp[,"nitrate"]),]
    results <- rbind(results, c(i, nrow(temp)))  
  }
  
  dimnames(results) = list(1:nrow(results), c("id","nobs"))
  data.frame(results)
}