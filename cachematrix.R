## These two functions work to calculate the inverse of a matrix. The makeCacheMatrix function makes the special matrix and cacheSolve function 
## calculates the inverse matrix.

## Function that makes the special matrix before calling cacheSolve function to get the inverse of matrix. It contains list containing functions to 
## get/set the value of matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that returns the inverse of the matrix passed created via makeCacheMatrix function. It calculates inverse only when cache doesnot contain 
## previously calculated inverse. Otherwise returns the cached inverse for matrix

cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}