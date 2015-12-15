rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  hospital_outcomes <- read.csv('outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
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
  
  ## Fetch all hospitals for the state
  state_hospital_outcomes <- subset(hospital_outcomes, State == state)
  
  
  ## Get the desired outcome column#
  if (outcome == "heart attack") {
    outcome_column <- 11
  } 
  
  if (outcome == "heart failure") {
    outcome_column <- 17
  }
  
  if (outcome == "pneumonia") {
    outcome_column <- 23
  }
  
  if (is.numeric(num)) {
    if (length(state_hospital_outcomes[,2]) < num) {
      return (NA)
    }
  }
  
  ## Get rid of NA's from the data
  desired_column <- as.numeric(state_hospital_outcomes[,outcome_column])
  good <- !is.na(desired_column)
  state_hospital_outcomes <- state_hospital_outcomes[good,]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  ## Get the rows with min outcome values
  desired_column <- as.numeric(state_hospital_outcomes[,outcome_column])
  desired_rows <- which(desired_column == min(desired_column))
  candidate_hospitals <- state_hospital_outcomes[desired_rows,2]
  
  ## Sort by the hospital names and 
  ## return the first one from the sorted list
  if (length(candidate_hospitals) > 1) {
    candidate_hospitals <- sort(candidate_hospitals)
  }  
  
  if (is.character(num)) {
    if (num == 'best') {
      num = 1
    } else if (num == 'worst') {
      num = length(candidate_hospitals[,desired_column])
    }
  } 
  # Return the hospital name with the outcome ranking
  candidate_hospitals[num, 2]
}