# This pair of functions cache the inverse of a 'special matrix'
# created with the 'makeCacheMatrix'
# 'Special matrix' is equivalent to the normal version of the same matrix
# but it kind of magic coz 'cacheSolve' can calculate its inverse and cache it
# These functions only worked together. DON'T PASS A 'NORMAL MATRIX' TO 'cacheSolve'

## This function creates a special "matrix" object
## that can cache its inverse.

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


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
