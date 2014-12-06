## These functions work together to allow the caching of an inverse of
## a matrix, since matrix inversion can be a costly operation

## makeCacheMatrix takes an initial value for the underlying matrix,
## and returns a list of functions: allowing you to set and get the
## underlying matrix, and to set and get the inverse of the matrix.
## Setting the underlying matrix automatically clears the stored
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set, get=get, setsolve = setsolve, getsolve=getsolve)
}


## cacheSolve takes the result of a call to makeCacheMatrix and returns 
## the inverse of the underlying matrix stored there.  It will return
## the cached value if it exists, otherwise it will do the computation and
## cache it before returning it.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}
