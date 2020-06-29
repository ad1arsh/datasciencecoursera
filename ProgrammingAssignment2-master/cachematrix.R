## The pair of functions below caches the inverse of a supplied matrix object,
##preventing repeated computation


## This function generates a matrix object that allows its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
  
  invrse <- NULL
  set <- function(y) {
    x <<- y
    invrse <<- NULL
  }
  get <- function() x
  setinverse <- function(invrse) invrse <<- inverse
  getinverse <- function() invrse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the matrix object returned by the makeCacheMatrix function above.
## In the event that the inverse is already calculated, cacheSolve returns the inverse stored in the cache

cacheSolve <- function(x, ...) {
  invrse <- x$getinverse()
  if(!is.null(invrse)) {
    message("getting cached data")
    return(invrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinverse(invrse)
  invrse
}
