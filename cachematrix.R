## Assignment: Caching the Inverse of a Matrix

## Given that matrix inversion is usually a costly computation, there is some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.


## This function creates a special "matrix" object that can cache its inverse,
## get the matrix, set a new matrix and return its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
