corr <- function(directory, threshold = 0){
  
  findComplete <- complete("specdata", 1:332)
  nobs <- findComplete$nobs
  
  ids <- findComplete$id[nobs > threshold]
  
  idLength <- length(ids)
  corrVector <- rep(0, idLength)
  
  counter <-1
  for(i in ids){
    filename <-''
    if(i>0 && i<10){
      filename <- paste(c(directory, '/00' , i , '.csv'), collapse = '')
    }else if(i>9 && i<100){
      filename <- paste(c(directory, '/0' , i , '.csv'), collapse = '')
    }else{
      filename <- paste(c(directory , '/', i , '.csv'), collapse = '')
    }
    
    currentFile <- read.csv(filename, sep=",")
    corrVector[counter] <- cor(currentFile$sulfate, currentFile$nitrate, use="complete.obs")
    counter <- counter + 1
  }
  
  result <- corrVector
  result
  
}