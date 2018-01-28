## R Programming Assignment 2 - Lexical Scoping - Jan 27 2018 4.48pm Pacific Time
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
############### Checking the Function ###########################

## m <- matrix(rnorm(25),5,5)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)
##[,1]       [,2]        [,3]       [,4]       [,5]
##[1,] -3.1344863  1.2517086  0.93772079  0.3366642  3.2996046
##[2,]  3.2996902 -1.9290560 -0.59473120 -0.4914235 -3.5789927
##[3,]  0.3924716  0.1677839  0.16725333 -0.3143919 -0.6267145
##[4,] -1.8907002  1.3615218  0.88697783  0.2711339  1.1578662
##[5,]  0.3773693  0.4090822  0.04995728  0.1527799  0.2476439
> 