## The following code creates a pair of functions that
## cache the inverse of a matrix. The first function `makeCacheMatrix` creates a special "matrix"
## that can cache its inverse. The second function `cacheSolve` computes the inverse of the special
## "matrix" returned by the `makeCacheMatrix` function. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache. It is assumed that the input matrix is invertible.


## The function `makeCacheMatrix` below creates a special "matrix" 
## that can cache its inverse. It returns a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(xi) xinv <<- xi
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" x
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix xinv that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached inverse")
                return(xinv)
        }
        xmat <- x$get()
        xinv <- solve(xmat, ...)
        x$setinv(xinv)
        xinv
}
