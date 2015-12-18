rankall <- function(outcome, num = "best") {
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital_data[,11] <- suppressWarnings(as.numeric(hospital_data[,11]))
  hospital_data[,17] <- suppressWarnings(as.numeric(hospital_data[,17]))
  hospital_data[,23] <- suppressWarnings(as.numeric(hospital_data[,23]))
  
  expected_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% expected_outcomes)) {
    stop(paste("Error in best(", state, ",", outcome, ") : invalid outcome", sep = ""))
  }
  
  #if (!(state %in% hospital_data$State)) {
   # stop(paste("Error in best(", state, ",", outcome, ") : invalid state", sep = ""))
  #}
  
  ll <- list()
  for (state in sort(unique(hospital_data$State))) {
    state_hospital_data <- subset(hospital_data, State == state)
    state_hospital_data[,11] <- suppressWarnings(as.numeric(state_hospital_data[,11]))
    state_hospital_data[,17] <- suppressWarnings(as.numeric(state_hospital_data[,17]))
    state_hospital_data[,23] <- suppressWarnings(as.numeric(state_hospital_data[,23]))
    
    if (outcome == "heart attack") {
      state_hospital_data <- state_hospital_data[order(state_hospital_data[,11],
                                                       state_hospital_data[,2],
                                                       na.last=NA),]
    } else if (outcome == "heart failure") {
      state_hospital_data <- state_hospital_data[order(state_hospital_data[,17],
                                                       state_hospital_data[,2],
                                                       na.last=NA),]
    } else {
      state_hospital_data <- state_hospital_data[order(state_hospital_data[,23],
                                                       state_hospital_data[,2],
                                                       na.last=NA),]
    }
    
    if (num == "best") {
      ll[state] <- as.vector(state_hospital_data[1,2])
    } else if (num == "worst") {
      ll[state] <- as.vector(state_hospital_data[nrow(state_hospital_data),2])
    } else if (num > nrow(state_hospital_data)) {
      ll[state] <- c("NA")
    } else {
      ll[state] <- as.vector(state_hospital_data[num,2])
    }
  }
  
  dataframe <- as.data.frame(do.call(rbind, ll))
  dataframe[,2] <- dataframe[,1]
  dataframe[,1] <- rownames(dataframe)
  colnames(dataframe) <- c("state", "hospital")
  
  dataframe <- data.frame(dataframe)
}  