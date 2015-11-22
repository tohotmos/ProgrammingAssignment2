# R functions is to cache time-consuming matrix inversion computations
# If the contents of a matrix are not changing, we may ache the inverted matrix so that when we need it again,
# it can be looked up in the cache rather than recomputed.


## Function 'makeCacheMatrix()':
## Creates a special "vector", which is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)

}


## Function 'cacheSolve()':
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s

}
