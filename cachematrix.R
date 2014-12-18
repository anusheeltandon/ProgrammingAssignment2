## This module aims at reducing the time to run iterative matrix inversion 
## process by caching the results. If the inverse of a matrix is already 
## calculated and available in the cache then it won't be taken through the 
## calculation process again.

## Users of the module are advised to use 'cacheSolve' function over a list of
## matrices to calculate their inverse(s) [Call 'cacheSolve' from within a loop
## that is iterating over list of matrices].
################################################################################


## creates a special matrix that caches its mean

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


## cacheSolve method iterates over a list of matrices and calculate their inverse
## if inverse for a matrix is already available in the cache then it skips 
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