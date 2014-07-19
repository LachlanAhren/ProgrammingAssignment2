## Pair of functions to enable cacheing of the inverse of a matrix, the calculation of the inverse of a matrix
## and the utilization of the cached values when available

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## variable to hold inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    ## set i to solved inverse from cacheSolve
    setinverse <- function(inverse) i <<- inverse
    ## retrieve cached i (NULL if not set, otherwise computed inverse)
    getinverse <- function() i
    ## create vector with getters and setters for the matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of the matrix, from cache when possible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## Return inverse if non-null
    if(!is.null(m)) {
        ##message("getting cached data")
        return(m)
    }
    ## Else retrieve matrix elements, calculate inverse, cache answer, and return answer
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
