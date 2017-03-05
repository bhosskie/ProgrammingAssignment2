## This R script defines the following functions
##
## 1. makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: Computes the inverse of a matrix created by the
##    "makeCacheMatrix" function. If the inverse of the matrix is available in the
##    cache, it is retrived from the cache instead of being recomputed.

## Creates a special "matrix" object that has the ability to cache its inverse.
## This object has the following functions
##
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setInverse - set the value of the inverse of the matrix
## 4. getInverse - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the given matrix 'x'. If the inverse of x is found 
## in the cache, the cached value is returned. Else, the inverse is computed,
## added to the cache and then returned.

cacheSolve <- function(x, ...) {
    matInverse <- x$getInverse()
    if(!is.null(matInverse)) {
        message("Getting matrix inverse from the cache...")
        return(matInverse)
    }
    matrixData <- x$get()
    matInverse <- solve(matrixData)
    x$setInverse(matInverse)
    matInverse
}
