best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  allStates = unique(data[,7])
  if((state %in% allStates)== FALSE){
    stop("invalid state")
  }
  
  if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  #11, 17, 23
  ## Return hospital name in that state with lowest 30-day death rate
  hospitalName <- ""
  if(outcome == "heart attack")
  {
    data[,11] <- as.numeric(data[, 11])
    myMin <- min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[data$State==state], na.rm = TRUE)
    myIndex <- which(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == myMin & data$State==state)
    hospitalName <- data[myIndex,2]
  }
  else if(outcome == "heart failure"){
    data[,17] <- as.numeric(data[, 17])
    myMin <- min(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[data$State==state], na.rm = TRUE)
    myIndex <- which(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == myMin & data$State==state)
    hospitalName <- data[myIndex,2]
  }
  else if(outcome == "pneumonia"){
    data[,23] <- as.numeric(data[, 23])
    myMin <- min(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[data$State==state], na.rm = TRUE)
    myIndex <- which(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == myMin & data$State==state)
    hospitalName <- data[myIndex,2]
  }
  
  hospitalName
}