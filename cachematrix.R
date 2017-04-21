## Put comments here that give an overall description of what your
## functions do
## MakeCacheMatrix creates a special matrix that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Write a short comment describing this function: Use 4 functions to create an special invertible Matrix into the cache to find the inverse later quicker.

makeCacheMatrix <- function(x = matrix()) {
                      m <- NULL
                      set <- function(y) {
                            x <<- y
                            m <<- NULL
                      }
                      get <- function() x
                      setinverse <- function(solve) m <<- solve
                      getinverse <- function() m
                      list(set = set, get = get,
                            setinverse = setinverse,
                            getinverse = getinverse)
}


## Write a short comment describing this function: cacheSolve computes the inverse of the special “matrix” created by makeCacheMatrix. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated the function does it and store it into the cache.

cacheSolve <- function(x, ...) {
                      m <- x$getinverse()
                      if(!is.null(m)) {
                            message("getting cached data")
                            return(m)
                      }
                      data <- x$get()
                      m <- solve(data, ...)
                      x$setinverse(m)
                      m
}
