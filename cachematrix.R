## This pair of functions caches the inverse of a matrix so that it
## does not have to be computed repeatedly.

## makeCacheMatrix creates a special matrix object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the value of the matrix
    set <- function(y) { 
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x 
    ## set the inverse of matrix x
    setsolve <- function(solve) m <<- solve 
    ## get the inverse of matrix x
    getsolve <- function() m 
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
            ## check whether inverse matrix has been cached
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
