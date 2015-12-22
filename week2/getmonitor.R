getmonitor <- function(id, directory, summarize = FALSE) {
  
   id <- sapply(id, function(x) sprintf("%03d", as.numeric(x)))
 
  file <- paste(id, ".csv", sep = "")
  file <- paste(directory, file, sep = "/")

   data <- read.csv(file)
   
   if (summarize)
     print(summary(data))
     data
}