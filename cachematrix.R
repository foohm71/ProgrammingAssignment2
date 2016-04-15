## Functions for Assignment2 of R course

## Creates a list of functions of a given matrix and the functions to set and get its cached inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(x) inv <<- x
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This solves for the inverse of the matrix given the cache matrix list and stores it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

