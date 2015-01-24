## My implementation of the assigned task.

## This function creates an object (a list), that holds a value of
## a matrix, it's inverse, and four functions to manipulate them

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    
    ## function for setting new array
    set <- function(y)
    {
        x <<- y
        cachedInverse <<- NULL
    }
    
    ## returns array
    get <- function() x
    
    ## returns inverse array
    getInverse <- function() cachedInverse
    
    ## sets inverse array
    setInverse <- function(inverse)
    {
        cachedInverse <<- y
    }
    
    ## return list of functions to manipulate data in this object
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of the CacheMatrix object.
## If cached value is not present, it's being calculated and stored.
## Then that value is returned.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    ## if inverse missing, solve and store it
    if (is.null(inverse))
    {
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    }
    
    ## return inverse - either cached or fresh solved
    inverse
}
