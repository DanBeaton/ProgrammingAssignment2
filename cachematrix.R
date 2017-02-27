## These functions describe caching of a matrix calculation
## to allow for an already calcualted value be stored for later use
## and prevent unnecessary recalculations

## this function creates the cached value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## this function retrives the cached value for use in calculations

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- mean(mat.data, ...)
  x$setinv(inv)
  return(inv)

}



