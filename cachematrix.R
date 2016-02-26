# Cache the Inverse of a Matrix 
# Matrix inversion is a costly computation, so the functions below provide a 
# cache of a matrix inversion to avoid computing it repeatedly.
#
# Written Feb 25, 2016 by Justin Geiman
#

# Create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    # x is a matrix (assumed to be invertible)
    
    minv <- NULL # matrix inverse has not been computed yet
    # set the original matrix 
    set <- function(y) {
        # assign the argument to the original matrix
        x <<- y
        # clear the inverse variable
        minv <<- NULL
    }
    # get the original matrix
    get <- function() x
    # set the inverse of the matrix in the cache
    setinv <- function(inv) minv <<- inv  
    # return the inverse of the matrix from the cache
    getinv <- function() minv 
    # return a list of functions for special CacheMatrix object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# Compute the inverse of a matrix, or return the inverse from the cache 
# if the inverse has already been computed. 
cacheSolve <- function(x, ...) {
    # x is a special matrix object from function makeCacheMatrix

    # attempt to retrieve inverse from cache
    minv <- x$getinv()
    if(!is.null(minv)) {
        # inverse found in cache
        message("getting cached inverse")
        return(minv)
    }
    # since the inverse was not found in the cache
    # retrieve the original matrix from x, and compute the inverse
    mat <- x$get()
    minv <- solve(mat, ...)
    # save the inverse in the cache
    x$setinv(minv)
    ## Return a matrix that is the inverse of 'x'
    minv
}
