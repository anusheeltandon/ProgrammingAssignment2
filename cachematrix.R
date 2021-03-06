## This module aims at reducing the time to run matrix inversion 
## process for a list of matrices by caching the results. 
## If the inverse of a matrix is already 
## calculated and available in the cache then it won't be taken through the 
## calculation process again.

## Users of the module are advised to use 'cacheSolve' function over a list of
## matrices to calculate their inverse(s) [Call 'cacheSolve' from within a loop
## that is iterating over list of matrices that were created using 
## makeCacheMatrix].
################################################################################


## creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        matrixCopy <- x
        getCache <- function() {
                cachedInverse
        }
        setCache(calculatedInverse) {
                cachedInverse <<- calculatedInverse
        }
        getMatrix <- function() {
                matrixCopy
        }
}


## cacheSolve method is called repeatedly from a loop iterating over a list of
## matrix objects (created using makeCachedMatrix method). It calculates the 
## inverse of a given matrix object. 
## But,if inverse for a matrix is already available in the cache then it skips 
## calculation and picks/uses the cached value instead

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.null(x) &&
                    !is.null(x$getMatrix())) {
                cachedInverseCopy <- x$getCache()
                if (is.null(cachedInverseCopy)) {
                        calculatedInverse = solve(x$getMatrix())
                        x$setCache(calculatedInverse)
                        return calculatedInverse
                }
                else {
                        message("getting cached data");
                        return cachedInverseCopy
                }
                
        }
}