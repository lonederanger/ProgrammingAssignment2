## The following set of two functions is based on the example, provided with the Programming Assignment 2.
## It solves square matrix and caches the inversion. 
## If the same calculation is requested again, it returns the inverted matrix from the cache with comment.

## First function receives an square matrix and creates 4 subfunctions: get, set, getinv, setinv
## Objective of these subfuctions is to store input matrix and its inversion (result of the second step)
makeCacheMatrix<- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solved) inv <<- solved
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Second function checks if matrix is already solved and returns the previously calculated inversion if yes
## Else, it solves the matrix and passes its inversion to "setinv" subfunction to store it for further calculations
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverted matrix")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setinv(inv)
    inv
}

## To be honest, I didn't like this assgnment. Hope you did. Grigory