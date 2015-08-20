## Similar to the example, the overally function of this program is to find the inverse of
## of a matrix. It then stores that inverse in the cache. If the inverse is called again,
## it will access the cache instead and pull the inverse from there.

## This function takes a matrix and creates a special list based on that matrix. This stores
## the function, as well as stores the inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function first checks if the inverse is stored in the memory. If not, it calculates
## the inverse. It then stores it in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
