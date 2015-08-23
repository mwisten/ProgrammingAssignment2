## File: cachematrix.R (forked from 'https://github.com/rdpeng/ProgrammingAssignment2'
## Functions: makeCacheMatrix, cacheSolve
## Coded by:	Manila Wisten
## Date:	22 August 2015

## Description: A pair of functions that cache the inverse of a matrix
##		Assumes the input matrix always has an inverse
##		Returns the inverse.



## 'makeCacheMatrix' function creates a special "matrix" object that can 
## cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## get current input matrix
        setinverse <- function(xinv) m <<- xinv         ## Inverse xinv calculated elsewhere
        getinverse <- function() m                      ## m is cached inverse of x
                                                        ## Return special "matrix"
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'cacheSolve' function computes the inverse of the special "matrix" returned 
## by 'makeCacheMatrix' above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 

        mx <- x$getinverse()
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$setinverse(mx)
        mx
        
}
