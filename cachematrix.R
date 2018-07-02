## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #if(!is.matrix(x)) stop("Parameter must be a square matrix") #| nrow(x)!=ncol(x)
    inversex <- NULL
    set <- function(setx) {
        x <<- setx
        inversex <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversex <<- inverse
    getinverse <- function() inversex
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversex <- x$getinverse()
    if (!is.null(inversex)) {
        message("cached result:")
        return (inversex)
    }
    matrixdata <- x$get()
    inversex <- solve(matrixdata, ...)
    x$setinverse(inversex)
    inversex
}
