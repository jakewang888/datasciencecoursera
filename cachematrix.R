## Make a function to return a cached and inverted matrix for a given input 
## matrix 

## make a cached matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolved <- function(solved) s <<- solved
    getSolved <- function() s
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## Solve the given matrix
cacheSolve <- function(x, ...) {
    s <- x$getSolved()
    if(!is.null(s)) {
        message("getting cached matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolved(s)
    s
}