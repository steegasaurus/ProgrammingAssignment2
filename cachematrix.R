## Create an object containing a matrix and ability to cache
## its inverse with another function calculating inverse using
## that special object.

## Creates matrix object containing ability to cache inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse){I <<- inverse}
    getInverse <- function(){I}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates inverse of matrix calling the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getInverse()
    if(!is.null(I)){
        message('Getting cached data.')
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}