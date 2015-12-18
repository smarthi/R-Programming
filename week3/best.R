best <- function(state, outcome) {
  ## Read outcome data
  hospital_outcomes <- read.csv("outcome-of-care-measures.csv")
  
  ## check that state and outcome are valid
  states <- unique(hospital_outcomes[,7])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check if provided state is valid
  if (!state %in% states) {
    stop("Invalid state")
  }
  
  ## Check if provided outcome is valid
  if (!outcome %in% outcomes) {
    stop("Invalid Outcome")
  }
  
  ## get the subset of data for specified state
  state_hospital_outcomes <- subset(hospital_outcomes, State == state)
  
  ## Get the desired 30-day outcome column#
  if (outcome == "heart attack") {
    outcome_column <- 11
  } 
  
  if (outcome == "heart failure") {
    outcome_column <- 17
  }
  
  if (outcome == "pneumonia") {
    outcome_column <- 23
  }
  
  ## Get rid of NA's from the data
  desired_column <- as.numeric(state_hospital_outcomes[,outcome_column])
  ## Return the hospital name in the state with the lowest
  ## 30-day death rate
  
  ## Get the rows with min outcome values
  desired_rows <- which.min(desired_column)
  print(desired_rows)
  print(state_hospital_outcomes[desired_rows, 1:3])
  candidate_hospitals <- state_hospital_outcomes[desired_rows,2]
  
  ## Sort by the hospital names and 
  ## return the first one from the sorted list
  if (length(candidate_hospitals) > 1) {
    candidate_hospitals_sorted <- sort(unique(candidate_hospitals))
    as.vector(candidate_hospitals_sorted[1])
  } else {
    as.vector(candidate_hospitals[1])
  }
}