## Make a function to return a cached and inverted matrix for a 
## given matrix 

## make a cache matrix function with setter and getter functions 
makeCacheMatrix <- function(x = matrix()) {
    ## s is the cached and inverted matrix variable
    s <- NULL
    
    ## Assign a new matrix(y) to x and clear the cached matrix(s) 
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## Return the original matrix(x)
    get <- function() x
    
    ## Assign the inverted matrix to the cached matrix(s)
    setSolved <- function(solved) s <<- solved
    
    ## Return the cached and inverted matrix (s)
    getSolved <- function() s
    
    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## Once the given matrix (x) had beed solved, return the cached 
## and inverted matrix instead.
cacheSolve <- function(x, ...) {
    
    ## Evaluate weather the cached matrix already exists
    s <- x$getSolved()
    if(!is.null(s)) {
        message("getting cached matrix")
        ## Return the cached matrix if it's already there
        return(s)
    }
    
    ## Get the original matrix (x)
    data <- x$get()
    
    ## Solve the original matrix 
    s <- solve(data, ...)
    
    ## Setup cached matrix
    x$setSolved(s)
    
    ## Return cached and inverted matrix
    return(s)
}