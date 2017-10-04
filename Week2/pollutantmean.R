pollutantmean <- function(directory, pollutant, id=1:332){
  
  allData <- c()
  for(i in id)
  {
      print(i)
      filename <-''
      if(i>0 && i<10){
        filename <- paste(c(directory, '/00' , i , '.csv'), collapse = '')
      }else if(i>9 && i<100){
        filename <- paste(c(directory, '/0' , i , '.csv'), collapse = '')
      }else{
        filename <- paste(c(directory , '/', i , '.csv'), collapse = '')
      }

    hw2data <- read.csv(filename, sep=",")
    polluData <- hw2data[,pollutant, drop=FALSE]

    bad <- is.na(polluData)
    polluData <- polluData[!bad]
    
    allData <- c(allData, polluData)
    
  }
  
  meanValue <- mean(allData, na.rm = TRUE)
  meanValue
}