## R Programming Course - Assignment 2
## Function makeCacheMatrix(x) generates an object which is stored as a list.
##    The list consists of four methods/functions that could be used to retrieve and modify
##    matrix x and its inverse.

## Function cacheSolve(x,...) returns the cached inverse of a matrix.

## Function makeCacheMatrix(x)
##  Argument: x - A matrix
##  Returns a list consisting of four functions: set, get, setInverse, and getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Function cacheSolve(x,...)
##  Argument: x - A special list storing four functions to access and modify matrix x and its inverse
##  Returns the cached inverse associated with list x

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
