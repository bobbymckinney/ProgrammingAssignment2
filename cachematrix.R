## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## function that sets the data matrix x 
    ## and NUlls the inverse matrix m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## function that returns the data matrix x
    get <- function() x
    ## function that sets the inverse matrix m
    setSolve <- function(solve) m <<- solve
    ## function that returns the inverse matrix m
    getSolve <- function() m
    
    ## return a list of functions to be used in cacheSolve
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if the inverse is already cached
    m <- x$getSolve()
    if(!is.null(m)) {
        ## return the cached inverse matrix if it isn't NULL
        message("getting cached matrix")
        return(m)
    }
    
    ## Otherwise get the data 
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
