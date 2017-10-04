## Creat a list object "makeCacheMatrix" that contains elements of
## "get", "setinv" and "getinv"
makeCacheMatrix <- function(x=matrix()) {
  ## set variable i to be NULL for the argument used later in !is.null
  i<-NULL
  ## function "set" allows us to feed the function through y
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  ## function "get" will store the matrix input
  get<-function() x
  ## function "setinv" will set the value of the inverse calculated
  ## in function "cacheSolve" x$setinv(i)
  setinv<-function(inv) i<<-inv
  ## function "getinv" allows to store the value of inverse and to be called
  ## in first line of function "cacheSolve"
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The following function examines if the inverse has been calculated
## If yes, the stored value will be returned
## If not, the inverse will be calculated

cacheSolve <- function(a) {
  ## call the value of getinv in function "makeCacheMatrix"
  i <- a$getinv()
  ## If the value called above is not null, it will return the message
  ## and the value. Otherwise, it will get the matrix from 
  ## function "get" and calculate the inverse of the matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- a$get()
  i <- solve(data)
  a$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}