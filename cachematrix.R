## The 'makeCacheMatrix' and 'cacheSolve' functions provide capabilities 
# for caching the result of a computationally-intensive operation, 
# which is calculating inverse of a matrix.
# This capability can be useful if calculating an inverse is executed multiple
# times for the same matrix (e.g. in a loop).
#
# Simply use 'makeCacheMatrix' passing a matrix as an argument.
# Then use 'cacheSolve' instead of the standard 'solve' function, passing in
# the object returned by 'makeCacheMatrix' as the argument.

## Wraps the given matrix and adds a field for storing cached matrix inverse.
# This function returns a list of 4 functions that can be used to
# manipulate the wrapped matrix: 
# 1. set - replaces the underlying matrix (also clears cache).
# 2. get - returns the underlying matrix.
# 3. setInverse - fills the inverse matrix cache.
# 4. getInverse - returns the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(matrixInverse) {
        cachedInverse <<- matrixInverse
    }
    
    getInverse <- function() {
        cachedInverse
    }
    
    list(set = set
         , get = get
         , setInverse = setInverse
         , getInverse = getInverse)
}


## Uses the cacheable matrix returned by 'makeCacheMatrix' to avoid 
# recomputing the inverse of the matrix. If cached inverse value is available
# then it is returned immediately. If it is not then the inverse is computed
# and stored in cache before returning.
#
# Parameter x is a list returned by 'makeCacheMatrix' that
# provides access to the underlying matrix as well as 
# its cached inverse, if available.
# Return value is the inverse matrix for the given matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getInverse()
    if (! is.null(matrixInverse)) {
        message("Getting cached inverse")
        return(matrixInverse)
    }
    
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
