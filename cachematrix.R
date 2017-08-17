## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL

    first <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    second <- function() x
    Inverse1 <- function(inverse) inverse <<- inverse
    Inverse2 <- function() inverse
    list(first = first,
         second = second,
         Inverse1 = Inverse1,
         Inverse2 = Inverse2)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("get cached data")
        return(inverse)
    }
    first <- x$get()
    inverse <- solve(first, ...)
    x$setInverse(inv)
    inverse
}
