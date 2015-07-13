## This script contains a pair of functions for creating a matrix with a cached inverse (makeCacheMatrix)
## and a solve function (cacheInverse) that checks whether the inverse is already cached before calculating it.
##
## Load and execute RUnit test suite using source("run_tests.R")

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Throw an exception if y is not a square matrix
  if (!isSquareMatrix(x)) {
    stop("Parameter must be a square n x n matrix")
  }
  
  ## initialize the cached inverse
  inv <- NULL
  
  ## $set: stores y as the value of the underlying matrix and resets the cached inverse
  set <- function(y) {
    ## Throw an exception if y is not a square matrix
    if (!isSquareMatrix(y)) {
      stop("Parameter must be a square n x n matrix")
    }
    x <<- y
    inv <<- NULL
  }
  
  ## $get: retrieves the underlying matrix x
  get <- function() {
    x
  }
  
  ## $setInverse: caches the inverse of x
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## $getInverse: returns the cached inverse of x
  getInverse <- function() {
    inv
  }
  
  ## return the special "matrix" as a list of functions
  list(
    set = set, 
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse)
}


## This function returns the inverse of x by either
## returning the cached solution if previously calculated
## or by calculating it

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## If the inverse was cached return it
  if (!is.null(inv)) {
    message("Returning cached value")
    return(inv)
  }
  
  ## Since the inverse was not cached, retrieve the matrix and solve for the inverse
  data <- x$get()
  inv <- solve(data)
  ## cache the inverse
  x$setInverse(inv)
  inv
}

## Utility function to check if an object is a square matrix
isSquareMatrix <- function(x) {
  is.matrix(x) && (nrow(x) == ncol(x))
}
