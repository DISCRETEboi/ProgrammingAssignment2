## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  mI <- NULL
  set <- function(y) {
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) mI <<- inv
  getInverse <- function() mI
  list(setf=set, getf=get, setInversef=setInverse, getInversef=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mI <- x$getInversef()
  if (!is.null(mI)) {
    message("getting cached data")
    return(mI)
  }
  data <- x$getf()
  mI <- solve(data, ...)
  x$setInversef(mI)
  mI
}
