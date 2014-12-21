## Second Programming Assignment in Coursera - R Programming
## written by Kaspar Jenni on 12/21/2014
##
## Lexical Scoping - Caching the inverse of a matrix
##
## makeCacheMatrix creates a list containing a function to:
## 1.set the value of a matrix
## 2.get the value of a matrix
## 3.set the value of the inverse
## 4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the object created with
## makeCacheMatrix, but first checks whether it has already been
## calculated. If so it gets the inverse from the cache and skips
## the calculation. Otherwise it calculates the inverse and sets
## the value of the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
