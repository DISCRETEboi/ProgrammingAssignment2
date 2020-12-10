## The function 'cacheSolve' calculates the inverse of a special matrix which has its
## inverse already cached by the 'makeCacheMatrix' function

## This function creates the special matrix

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


## This function computes the inverse of the special matrix and returns it

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
