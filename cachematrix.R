# The purpose of this file is to build a function to perform a potentially expensive computation,
# inverting a matrix, and cache it for future use.
# This file contains two functions, makeCacheMatrix and cacheSolve
# The first function builds a vector that contains several functions for storing the matrix,
# inverting it, and returning the cached copy when called.
# The second function exercises the first.
# Jeff Holtzman, 2016-12-04


# build a vector for managing a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Return a matrix that is the inverse of 'x', either computed or cached
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
