## The intention of this assignment is to write
## a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve calculates the inverse of the special "matrix" returned by
## makeCacheMatrix, given that is has not been calculated before. 
## If this is the case (calculated before), cacheSolve will 
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        
        m
}
