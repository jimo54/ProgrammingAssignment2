## These two functions make it possible to create and
## store an invertible matrix, as well as its inverse. 
## The functions take advantage of R's ability to cache
## values, using the <<- operator, to avoid unnecessarily 
## repeating computationally-expensive calculations such 
## as, in this instance, calculating the inverse of a
## matrix.

## The makeCacheMatrix() function takes an invertible 
## matrix as input and returns a list of functions for 
## getting and setting/replacing the matrix by caching 
## its value. Functions for getting and setting the
## matrix's inverse are also provided.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve() function takes a special "matrix" 
## object as input and uses its functions to calculate 
## and/or store the inverse of the matrix object. Until 
## or unless the matrix changes, the inverse is calculated 
## just once and then cached, using the matrix object's 
## csetinverse() function. On subsequent calls to this 
## function, a cached copy of inverse matrix is returned, 
## using the matrix object's getinverse() function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
