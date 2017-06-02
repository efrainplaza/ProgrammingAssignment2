## These are the functions to store inverse of Matrix in Cache and retrieve from Cache

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## To test these functions run the following commands (after sourcing functions)
## AMatrix <- matrix(c(1,1,4,0,3,1,4,4,0),3,3)
## aMakeMatrix <- makeCacheMatrix(AMatrix)
## cacheSolve(aMakeMatrix)
## Before testing with other matrix verify it can be inverted, using solve(your Matrix)
## you should get values, if you get an error, the matrix cannot be inverted

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}