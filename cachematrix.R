## Caching the inverse of matrix
## The first function makeCacheMatrix function creates a special matrix 
## objective that can cache its inverse. 
## The second function cacheSolve computes the inverse of the special matrix 
## returned by makeCacheMatrix function. 

## makeCacheMatrix function sets and gets the matrix. Then sets and gets 
## the inverse of matrix.
## Function's argument has to be a square invertible matrix.
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

## cacheSolve function returns a matrix that is the inverse of 'x' using 
## Solve(x) function. It will first check if matrix has been already inversed. 
## If so, then it will retrieve the inverse from the cache. 
## Otherwise, it will caclculate the inverse and sets inverse by calling 
## setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("get cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}