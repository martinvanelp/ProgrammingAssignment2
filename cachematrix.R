## The makeCacheMatrix and cacheSolve functions beneath create a special
## "matrix" with cache functionality to lower computation time when
## calculating the inverse of a matrix.

# makeCacheMatrix creates a special "matrix", which is a list containing
# 4 functions to (1-2) set and get the value of the matrix and
# (3-4) set and get the inverse of the matrix.

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

# cacheSolve returns the inverse of the special "matrix" created with
# makeCacheMatrix. It gets this either from cache (getinverse) or
# computes it and then puts it in the cache (setinverse).

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
