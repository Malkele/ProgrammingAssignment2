## The following function computes and caches the inverse of a matrix. It 
##can be used to shorten computing time when repeated computations are required


## makeCacheMatrix creates and returns a matrix that will be stored in cache

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve retrieves inverse matrix from cache and if the matrix
## doesn't exist calculates the inverse of matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
 
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invrs)
  invrs
}
