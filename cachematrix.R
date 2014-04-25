## This file contains functions for working with matrices
## which can store the results of expensive operations.


## Creates a "wrapped" matrix which stores additional data.
## Other functions in this file use this to cache certain calculation results.
makeCacheMatrix <- function(x = matrix()) {
    # privately-scoped variable which will hold the memoized value
    i <- NULL 
    
    # updates input matrix
    set <- function(y) {
        x <<- y    # replace old matrix with new input
        i <<- NULL # reset memozied value to "empty"
    }
    
    # returns input matrix
    get <- function() x
    
    # updates the memoized value
    setcached <- function(y) i <<- y
    
    # returns the memoized value
    getcached <- function()  i
    
    # expose getter,setter functions to outside world
    list(get       = get, 
         set       = set, 
         getcached = getcached, 
         setcached = setcached)
}


## Computes the inverse of a "wrapped" square matrix, 
## but only if the data (of the underlying matrix) changes    
cacheSolve <- function(x, ...) {
    # get memoized inverse from "wrapped" matrix
    i <- x$getcached()
    
    if (!is.null(i)) {
        # inverse is not "empty", so return it to caller
        message("getting cached inverse")
        return(i) # early exit... no further processing needed
    } 
    
    # inverse was "empty", so...
    # get "unwrapped" matrix
    y <- x$get()
    # calculate inverse
    i <- solve(y, ...)
    # store inverse in "wrapped" matrix
    x$setcached(i)
    # return inverse to caller
    i
}
