## The following functions create and work on a special matrix
## representation calculating and cacheing its inverse.
## The special matrix object is represented as a list of four functions:
## 'get', 'set' to get and set the matrix data
## 'getinverse', 'setinverse' to get and set the inverse matrix data
## The "raw" data for the matrix and its inverse
## are stored within the environment of the function.

## Creates the special matrix object from the provided matrix argument
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

## Calculates and returns the inverse of the matrix
## or returns the cached value if already computed.
## As a side effect, it stores the newly calculated
## inverse in the special matrix object.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached matrix inverse")
    return(inverse)
  }
  matrix_data <- x$get()
  inverse <- solve(matrix_data)
  x$setinverse(inverse)
  return(inverse)
}
