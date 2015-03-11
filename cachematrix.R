## This file contains functions that cache the inverse of a matrix.
## - makeCacheMatrix
## - cacheSolve
## 
## Example: get the inverse of matrix x:
## > x <- matrix(c(4, 3, 3, 2), 2, 2)
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## makeCacheMatrix
## 
## This function creates an extended matrix object that includes a cache of 
## the inverse of the matrix.
## 
## Arguments:
## - x: the square invertible matrix to cache (optional).
##
## Returns a list of:
## - set: function to set the matrix.
## - get: function to retrieve the matrix.
## - setinverse: function to cache the inverse of the matrix.
## - getinverse: function to retrieve the cached inverse of the matrix.
## Note: getinverse() returns NULL if no inverse has been cached.
makeCacheMatrix <- function(x = matrix()) {
    ## Assign a default NULL value to the cached inverse.
    m <- NULL
    
    ## Sets the matrix. Also resets the cached inverse.
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    
    ## Returns the matrix.
    get <- function() {
        x
    }
    
    ## Sets the inverse of the matrix.
    setinverse <- function(inverse) {
        m <<- inverse
    }
    
    ## Returns the cached inverse of the matrix, if any.
    getinverse <- function() {
        m
    }
    
    ## Return.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
## 
## This function returns the inverse of a matrix-like object created with 
## makeCacheMatrix(). The function attempts to retrieve the inverse from cache, 
## or computes it if no cache is available.
## 
## Arguments:
## - x: the special matrix created with makeCacheMatrix (required).
## - ...: further arguments passed to the solve() function used to compute the 
##        inverse of the matrix (optional).
##
## Returns:
## - Inverse of the matrix.
cacheSolve <- function(x, ...) {
    ## Retrieves the cached inverse of the matrix.
    m <- x$getinverse()
    
    ## Check if the cached inverse is NULL (no cache) or not.
    if (!is.null(m)) {
        ## If the cached inverse is not NULL, nothing further needs to be done.
        message("Using cached inverse.")
    }
    else {
        ## If the cached inverse is NULL, generate the inverse and cache it.
        m <- solve(x$get(), ...)
        x$setinverse(m)
    }
    
    ## Return the inverse of the matrix.
    m
}