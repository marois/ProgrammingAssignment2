test.makeCacheMatrix <- function() {
  x <- matrix(1:4, nrow=2, ncol = 2)
  cx <- makeCacheMatrix(x)
  ## Check that makeCacheMatrix returns a list of functions
  checkEquals(class(cx), "list")
  checkEquals(class(cx$set), "function")
  checkEquals(class(cx$get), "function")
  checkEquals(class(cx$setInverse), "function")
  checkEquals(class(cx$getInverse), "function")
  
  ## check that the cache value is initially empty
  checkTrue(is.null(cx$getInverse()))
  
  ##check that get function returns x
  checkEquals(cx$get(), x)
  
  ##check that getInverse function returns the value set by setInverse
  inv <- solve(x)
  cx$setInverse(inv)
  checkEquals(cx$getInverse(), inv)
  
  ##check that cache is now used
  msg <- NULL
  withCallingHandlers(cacheSolve(cx), message = function(w) {msg<<-w})
  checkEquals(msg[[1]], "Returning cached value\n")
  
  ##check that setting a new matrix clears the cache
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  r <- 8
  h <- hilbert(r)
  cx$set(h)
  checkTrue(is.null(cx$getInverse()))
  
  ##check that cacheSolve returns the inverse matrix
  identityMatrix <- diag(r)
  sh <- cacheSolve(cx)
  checkEquals(round(sh %*% h, 3), identityMatrix)
  
  ## check that an error is thrown if makeCacheMatrix is passed a non-square matrix is set
  msg <- NULL
  tryCatch(makeCacheMatrix(matrix(1:6, nrow=2, ncol=3)), error = function(w) {msg<<-w})
  checkEquals(msg[[1]], "Parameter must be a square n x n matrix")
  
}