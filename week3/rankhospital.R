rankhospital <- function(state, outcome, num = "best") {
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital_data[,11] <- suppressWarnings(as.numeric(hospital_data[,11]))
  hospital_data[,17] <- suppressWarnings(as.numeric(hospital_data[,17]))
  hospital_data[,23] <- suppressWarnings(as.numeric(hospital_data[,23]))
  
  if (!(state %in% hospital_data$State)) {
    stop(paste("Error in best(", state, ",", outcome, ") : invalid state", sep = ""))
  }
  
  expected_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% expected_outcomes)) {
    stop(paste("Error in best(", state, ",", outcome, ") : invalid outcome", sep = ""))
  }
  
  state_hospital_data <- subset(hospital_data, State == state)
  state_hospital_data[,11] <- suppressWarnings(as.numeric(state_hospital_data[,11]))
  state_hospital_data[,17] <- suppressWarnings(as.numeric(state_hospital_data[,17]))
  state_hospital_data[,23] <- suppressWarnings(as.numeric(state_hospital_data[,23]))
  
  if (outcome == "heart attack")
    state_hospital_data <- state_hospital_data[order(state_hospital_data[,11],
                                                     state_hospital_data[,2],
                                                     na.last=NA),]
  else if (outcome == "heart failure")
    state_hospital_data <- state_hospital_data[order(state_hospital_data[,17],
                                                     state_hospital_data[,2],
                                                     na.last=NA),]
  else
    state_hospital_data <- state_hospital_data[order(state_hospital_data[,23],
                                                     state_hospital_data[,2],
                                                     na.last=NA),]
  
  if (num == "best")
    num <- 1
  else if (num == "worst")
    num <- nrow(state_hospital_data)
  else if (num > nrow(state_hospital_data))
    return (c("NA"))
  
  return (as.vector(state_hospital_data[num,2]))
}