## Pair of functions below cache the inverse of a matrix.

## makeCacheMatrix - function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL ##assures that after changing matrix inverse will be cleared
  }
  get <- function() {
    x
  }
  setinverse <- function(inv) {
    inverse <<- inv
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##              If the inverse has already been calculated (and the matrix has not changed), 
##              then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("no need to recalculate, getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
