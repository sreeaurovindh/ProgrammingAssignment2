## This function makeCacheMatrix, gets a  matrix and calculates
## its inverse. Since calculation of the inverse is an expensive operation
## the makeCacheMatrix stores the value of the obtained inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve method calls the solveMethod of the 
## R package to calculate its inverse. Once it obtains
## its inverse it stores it. If the input matrix is
## not modified then it retrieves the stored value of the
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Obtaining Cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
