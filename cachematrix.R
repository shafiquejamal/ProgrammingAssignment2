## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# makeCacheMatrix takes a single arguement: a square invertible matrix, e.g.
#
# z <- makeCacheMatrix(matrix(c(1, 2, -1, 8), nrow=2, ncol=2, byrow = T))
#
# makeCacheMatrix stores a matrix, methods for getting and setting this matrix, and methods for getting and setting
# the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {

    inverserOfX <- NULL
    set <- function(y) {
        x <<- y
        inverserOfX <<- NULL
    }
    get <- function() x
    setInverseOfX <- function(newInverseOfX) inverserOfX <<- newInverseOfX
    getInverseOfX <- function() inverserOfX
    list(set = set, get = get, getInverseOfX = getInverseOfX, setInverseOfX = setInverseOfX)
}


## Write a short comment describing this function
#
# cacheSolve takes a single arguement: an object of type makeCacheMatrix. cacheSolve returns the inverse of the
# matrix stored in its arguement. It first checks to see whether the argument contains the inverse matrix. If so,
# cacheSovle returns this inverse matrix. Otherwise, cacheSolve calculates this inverse, stores this inverse in the 
# argument, and then returns this inverse. 
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverseOfX()
        if(!is.null(inv)) {
            message("getting cached data...")
            inv
        } else {
            message("cacluating...")
            xmatrix <- x$get()
            invOfX <- solve(xmatrix, ...)
            x$setInverseOfX(invOfX)
            invOfX
        }
}
