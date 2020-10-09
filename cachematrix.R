## Obtaining inverse of matrix by R

## Making Function to create a special matrix that can cache its inverse
## library(MASS) for calculating inverse for non-squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {
        inv <<- inverse
        }
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calcuating inverse of matrix using the cached data

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
