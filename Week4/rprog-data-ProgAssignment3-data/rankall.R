rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  allStates = unique(data[,7])
  allHospitals= unique(data[,2])
  
  if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  #retData <- data.frame(hospital, state)
  counter <- 1
  retData = as.data.frame(matrix(ncol = 2, nrow = length(allStates)))
  names(retData) = c("hospital", "state")
  for(state in allStates){
    
    hospitalName <- ""
    if(outcome == "heart attack")
    {
      data[,11] <- as.numeric(data[, 11])
      dataState <- data[data$State==state,]
      dataState <- dataState[!is.na(dataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c(2,7,11)]
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
    #hospitalName <- rankhospital(s,outcome, num)
    
    
    retData[counter,] = c(hospitalName, state)
    counter <- counter + 1
  }

  ## Return a data frame with the hospital names and the (abbreviated) state name
 sortedData <- retData[order(retData$state),] 
  
 sortedData
}