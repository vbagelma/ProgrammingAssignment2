##  Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieve the inverse from the cache by function getInverse
## If inverse doesn't exist, this function do inverse and retrieve matrix inversion by setInverse 
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
