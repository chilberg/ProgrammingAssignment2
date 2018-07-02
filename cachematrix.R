# https://github.com/chilberg/ProgrammingAssignment2.git

# makecachMatrix doesn't just return the matrix. It returns stored functions and one of the functions simply returns the initial matrix.
# The cache isn't actually a matrix, but functions that manipulate and return the matrix.

makeCacheMatrix <- function(x = matrix()) {
    # if(!is.matrix(x)) stop("Parameter must be a square matrix") #| nrow(x)!=ncol(x) # use when parameter is not specified
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


# cacheSolve tries to determine if the result has been stored in makeCacheMatrix and returns the result or calculates the result if NULL

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
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
