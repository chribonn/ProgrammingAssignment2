## Course ID: rprog-003
##
## Put comments here that give an overall description of what your
## functions do

## The following module provides a cached matrix inversion object to the user. 
## It takes a matrix passed to it as a parameter and stores it within the function object.
##
## Any calls to the projected function to retrieve the inverted matrix will cause 
## the function to return the one stored internally if defined or NULL if none have 
## been set.
##

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y, ...) {
        x <<- y
        invM <<- NULL
    }
    get <- function() {
            return (x)
    }
    setinverse <- function(solve) invM <<- solve
    getinverse <- function() {
        invM
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if (!is.null(invM))  {
        message ("returning cached matrix")
        return (invM)
    }
    else {
        message ("computing and caching inverse matrix")
        data <- x$get()
        invM <- solve(data, ...)
        x$setinverse(invM)
        invM
    }
}
