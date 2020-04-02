## Caching the inverse of a matrix

## makeCacheMatrix creates a special list containing a function to
# (1) set the value of the matrix
# (2) get the value of the matrix
# (3) set the value of the inverse
# (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
            x <<- y
            i <<- NULL
          }
          get <- function() x
          setSolve <- function(solve) i <<- solve
          getSolve <- function() i
          list(set = set, get = get,
               setSolve = setSolve,
               getSolve = getSolve)
}

## cacheSolve calculates the inversed matrix created with the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
          i <- x$getSolve()
          if(!is.null(i)) {
            message("getting cached data")
            return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setSolve(i)
          i
}
