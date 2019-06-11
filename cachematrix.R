# Week 3: Programming Assignment 2: Lexical Scoping
## Description: Calculating the inverse of a matrix can be computationally
## costly. 
## Thus, to reduce computation time, the pair of functions below cache the 
## inverse of a matrix

## makeCacheMatrix creates a "special" matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvmatrix <- function(solvematrix) m <<- solvematrix
        getInvmatrix <- function() m
        list(set = set, get = get, setInvmatrix = setInvmatrix, getInvmatrix = 
                     getInvmatrix)
}


## cacheSolve computes the inverse of a matrix returned by the makeCacheMatrix 
## function. If the inverse is already calculated, then cacheSolve returns the
## inverse matrix from the cache

cacheSolve <- function(x, ...){
        m <- x$getInvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInvmatrix(m)
        m
}
