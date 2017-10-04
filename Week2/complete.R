complete <- function(directory, id=1:332){
  idLength <- length(id)
  nrComplete <- rep(0, idLength)
  counter <- 1
  for(i in id)
  {
    filename <-''
    if(i>0 && i<10){
      filename <- paste(c(directory, '/00' , i , '.csv'), collapse = '')
    }else if(i>9 && i<100){
      filename <- paste(c(directory, '/0' , i , '.csv'), collapse = '')
    }else{
      filename <- paste(c(directory , '/', i , '.csv'), collapse = '')
    }
    
    hw2data <- read.csv(filename, sep=",")
    nrComplete[counter] <- sum(complete.cases(hw2data))
    counter <- counter + 1
  }
  
  idNobs <- data.frame(id = id, nobs = nrComplete)
  idNobs
}