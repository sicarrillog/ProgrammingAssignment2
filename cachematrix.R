#
#
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    list(get = get, set = set,
         setSolve = setSolve,
         getSolve = getSolve)
}
#
#
#
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
}