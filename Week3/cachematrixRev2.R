## Melanie D Young
## R Programming
## Programming Assignment 2

# makeCacheMatrix: This function creates a special matrix object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## return: a list of functions 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ## cache the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setmatrix<-function(inverse) m<<- inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## return: inverse of the matrix passed to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## call getmatrix()
  m<-x$getmatrix()
  ## does this return a value?
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## otherwise, m is NULL and I need to solve for the inverse of x
  n<-x$get()
  ## inverse
  m<-solve(n)
  ## reset the value of m
  x$setmatrix(m)
  ## return m
  return(m)
}