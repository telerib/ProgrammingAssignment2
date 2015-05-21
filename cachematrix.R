## Caches the inverse of a matrix.  Once inverse is calculated the first time,
## subsequent requests for the identical matrix return a cached value.

## Creates a list to store the matrix and its inverse, provide getter and setter
## Should be called on matrix before cacheMatrixSolve used

makeCacheMatrix <- function(x = matrix()) {
 		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() return(inv)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If inverse previously calculated for matrix x last in memory, returns cached
## inverse. Otherwise, calculates inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
