## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatix takes a matrix and calculates its inverse,
## storing the result in the enviornment's cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve check if the matrix has already been inverted
## and is stored in the environment's cache. If so, it returns that value;
## if not, the inverse is calculates.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
