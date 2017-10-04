## caching the calculated inverse of a matrix

## This function creates a special matrix which is a list of get,set, getinverse and setinverse functions
makeCacheMatrix <- function(x = matrix()) {
  inversex <- NULL
  set <- function(inmatrix){
    x <<- inmatrix
    inversex <<- NULL
  }
  get <- function() x
  setinverse <- function(inputinverse)  inversex <<- inputinverse
  getinverse <- function() inversex
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function either calculates the inverse of special matrix created using makeCacheMatrix() function 
## or it gets the inverse matrix from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversex <- x$getinverse()
  if(!is.null(inversex)){
    message("getting cached data")
    return(inversex)
  }
  data <- x$get()
  inversex <- solve(data, ...)
  x$setinverse(inversex)
  inversex
}

