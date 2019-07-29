## Cashing the Inverse of a Matrix:
## Rather than a computation that can be costly time wise, these functions cache the inverse of the matrix for quick recall
## raher than constant recalculation.

## This function creates a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x<<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}



## This function computers the inverse of the makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
