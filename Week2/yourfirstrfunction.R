add2 <- function(x, y){
  x+y
}

above10 <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x, n=10) {
  use <- x > n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}

pollutantmean <- function(directory, pollutant, id=1:332){
  
  allData <- c()
  for(i in id)
  {
    print(i)
    filename <-''
    # filename <- paste(c('specdata' , '/00' , id , '.csv'), collapse = '')
    if(i>0 && i<10){
      filename <- paste(c(directory, '/00' , i , '.csv'), collapse = '')
      print('here1')
    }else if(i>9 && i<99){
      filename <- paste(c(directory, '/0' , i , '.csv'), collapse = '')
      print('here2')
    }else{
      filename <- paste(c(directory , '/', i , '.csv'), collapse = '')
      print('here3')
    }
    
    
    hw2data <- read.csv(filename, sep=",")
    #print(hw2data)
    print(cat("Current nrow: ", nrow(hw2data)))
    print("maryam1")
    polluData <- hw2data[,pollutant, drop=FALSE]
    print('maryam jish')
    #print(polluData)
    
    print(cat('maryam nowww, nrow:', nrow(polluData)))
    #print(polluData)
    bad <- is.na(polluData)
    polluData <- polluData[!bad]
    print('after bad')
    #print(cat('maryam 4, nrow:', nrow(polluData)))
    #print(cat('maryam 5, nrow:', nrow(allData)))
    allData <- c(allData, polluData)
    #print(allData)
    #print(cat('maryam 6, nrow:', nrow(allData)))
}

  meanValue <- mean(allData, na.rm = TRUE)
  meanValue
}

readmyfile <- function(directory, id){
  filename <- paste(c(directory , '/00' , id , '.csv'), collapse = '')
  hw2data <- read.csv(filename)
  hw2data
}



