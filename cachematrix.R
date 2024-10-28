## the following functions will cache the inverse of a matrix

## the first function create a special"matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                        x <<- y
                        m <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) m <<- solve
              getsolve <- function() m
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}


## the second function computes the inverse of the "matrix" returned by the
## makeCacheMatrix defined above. If the inverse has already been calculated, 
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()
          if(!is.null(m)) {
                message("getting cache data")
                return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}
