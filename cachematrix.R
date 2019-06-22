## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# create a cached matrix object with functions supporting caching and uncaching both value and inverse

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function
# Solves the inverse of matrix x, caches it if not cached, else fetch the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}
