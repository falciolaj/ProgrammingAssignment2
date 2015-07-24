## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix creates custom matrix type capable of running four functions
        ## set stores the matrix in cache, get recalls the matrix
        ## setInverse and getInverse do the same but for the inverse of the original matrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()                   ## ask for the x matrix's cache
        if (!is.null(inv)) {                    ## if there is a cache, the inverse has been previously calculated
                message("getting cached data")  ## sent message indicating this is just cache
                return(inv)                     ## return the cache
        }
        mat <- x$get()          ## get the matrix used by makeCacheMatrix function 
        inv <- solve(mat, ...)  ## calculate the inverse of the matrix
        x$setInverse(inv)       ## store the inverse matrix in cache using the makeCacheMatrix set function
        inv                     ## return the inverse matrix
}
