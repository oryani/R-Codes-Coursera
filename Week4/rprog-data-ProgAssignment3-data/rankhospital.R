rankhospital <- function(state, outcome, num="best") {
  
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  hospitalName <- ""
  if(outcome == "heart attack")
  {
    data[,11] <- as.numeric(data[, 11])
    dataState <- data[data$State==state,]
    dataState <- dataState[!is.na(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c(2,7,11)]

    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    sortedData <- dataState[order(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, dataState$Hospital.Name, na.last = NA),] 
    if(num == "best"){
      hospitalName <- sortedData[1,1]
    }
    else if(num == "worst"){
      hospitalName <- sortedData[nrow(sortedData),1]
    }
    else if(num > nrow(sortedData)){
      hospitalName <- NA
    }
    else {
      hospitalName <- sortedData[num,1]
    }
    
  }
  else if(outcome == "heart failure"){
    data[,17] <- as.numeric(data[, 17])
    dataState <- data[data$State==state,]
    dataState <- dataState[!is.na(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),c(2,7,17)]
    
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    sortedData <- dataState[order(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, dataState$Hospital.Name),] 
    
    if(num == "best"){
      hospitalName <- sortedData[1,1]
    }
    else if(num == "worst"){
      hospitalName <- sortedData[nrow(sortedData),1]
    }
    else if(num > nrow(sortedData)){
      hospitalName <- NA
    }
    else {
      hospitalName <- sortedData[num,1]
    }
  }
  else if(outcome == "pneumonia"){
    data[,23] <- as.numeric(data[, 23])
    dataState <- data[data$State==state,]
    dataState <- dataState[!is.na(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),c(2,7,23)]
    
    sortedData <- dataState[order(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, dataState$Hospital.Name),] 
    if(num == "best"){
      hospitalName <- sortedData[1,1]
    }
    else if(num == "worst"){
      hospitalName <- sortedData[nrow(sortedData),1]
    }
    else if(num > nrow(sortedData)){
      hospitalName <- NA
    }
    else {
      hospitalName <- sortedData[num,1]
    }
  }
  hospitalName
}