## creates a spcieal matrix to cache the inverse value
## input is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## calculates the inverse of a matrix using the output from createCacheMatrix
## if matrix has not changed and inverse is already calculated the cached value will be returned

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
