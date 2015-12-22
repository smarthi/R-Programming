corr <- function(directory, threshold = 0) {
  
  complete_cases <- complete(directory)
  results <- numeric()
  
  for (i in seq_len(nrow(complete_cases))) {
     if (complete_cases[i, "nobs"] > threshold) {
         
         temp <- getmonitor(complete_cases[i,"id"], directory)
         temp <- temp[!is.na(temp[,"sulfate"]),]
         temp <- temp[!is.na(temp[,"nitrate"]),]         
         results <- c(results,cor(temp$sulfate, temp$nitrate))
     }  
  }
  
  results
  
}  