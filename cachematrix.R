## These two functions are used to create a special object
## that stores a numeric matrix and caches its inverse.

## This function creates a special "matrix" which is really
## just a list containing several functions that get and set
## the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse of the special
## "matrix" has already been computed. If so, it returns the
## inverse from the cache. Otherwise, it first computes the
## matrix inverse, stores it in the cache, and then returns it
## The message indicating that the inverse is being retrieved
## from the cache has been left for debugging purposes.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
