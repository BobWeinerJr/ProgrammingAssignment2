## These two functions work together to store, solve, and return 
## a matrix and its inverse.  After the inverse is calculated, 
## the cached value will be returned without recalculating. 


## Factory class that creates the list that will store and retrieve 
## values of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # store matrix new matrix 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # return stored matrix 
    get <- function() x
    
    # store provided value as inverse of x
    setinv <- function(x.inv) m <<- x.inv
    
    # return stored inverse of x
    getinv <- function() m
    
    # return list of 4 functions
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function that accepts a list created by makeCacheMatrix,
## calculates, and stores its inverse.  If the inverse is
## already been calculated, it returns the stored value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # calculating and storing inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
