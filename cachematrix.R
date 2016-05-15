## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" (a list, really), with the actual
## contents of the matrix, as well as cached inverse matrix.
## It also contains get/set methods/functions for the matrix and its
## inverse matrix respectively

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns inverse of the matrix created using the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
