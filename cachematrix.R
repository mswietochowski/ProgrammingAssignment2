## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y)
    {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    getInverse <- function() cachedInverse
    setInverse <- function(inverse)
    {
        cachedInverse <<- y
    }
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (is.null(inverse))
    {
        message("calculating inverse")
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    }
    else
    {
        message("returning cached value")
    }
    inverse
}
