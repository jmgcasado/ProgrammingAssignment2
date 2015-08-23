## Two functions that allow us to create an enhaced matrix which stores its inverse (makeCacheMatrix)
## and get its inverse cached previously or compute it again (cacheSolve).


## makeCacheMatrix function create an extended matrix that stores the matrix and its inverse.
## It's really contains a list of functions that represents this behaviour.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinv <- function(inverse= matrix()) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve function returns the inverse of our enhaced matrix built with makeCacheMatrix.
## The advantage is not always will compute this inverse if it's cached previously.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
      if(!is.null(inv)) {
              message("Getting cached data")
              return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinv(inv)
      inv
}