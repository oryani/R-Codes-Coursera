## Caching the Inverse of a Matrix - Programming Assignment 2
## Luca Amapane, November 20th 2015
## This file includes two functions to compute and cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Contains the functions to: 
##      Set the values of a matrix (set)
##      Get the values of a matrix (get)
##      Set the inverse of a matrix (setInv)
##      Get the inverse of a matrix (getInv)

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL 
  
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated then it retrievse the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  } else {
    
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInv(inv)
  inv
}