##------------------------------------------------------------
## makeCacheMatrix      --------------------------------------
##------------------------------------------------------------
##input:    x = numeric matrix (invertible).
##output:   list
##------------------------------------------------------------
##- enable caching funcions of matrix
##------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##------------------------------------------------------------
## cacheSolve           --------------------------------------
##------------------------------------------------------------
##input:    x= list output of makeCacheMatrix with input  
##             matrix to calculate inverse.
##output:   inverse of matrix
##------------------------------------------------------------
##- calculate inverse of matrix with caching
##------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
