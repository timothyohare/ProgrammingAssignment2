## These functions create a specialised matrix object that caches the inverse matrix

## This function creates the specialsed matrix object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This functon returns the matrix inverse. If it is available in the cache,
## it will return the cached version

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #d <- data.frame(x)
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
