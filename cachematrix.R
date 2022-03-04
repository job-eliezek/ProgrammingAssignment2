## Returns the inverse of a given matrix
## demonstrates lexical scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get matrix
  get <- function() x
  # set inverse
  setinverse <- function(setinv) inverse <<- setinv
  # get inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  # use cached inverse if exists
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise compute inverse and cache it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  # return inverse
  inverse
}
