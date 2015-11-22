## USING CLOSURES ##
## ============== ##

# These 2 functions are to cache time-consuming matrix inversion computations
# If the content of a matrix is not changing, we may cache the inverted matrix so that when we need it again,
# it can be looked up in the cache rather than recomputed.

# Example of use:
# > mat <- matrix(runif(16, -10, 10), 4, 4)
# > cmat <- makeCacheMatrix(mat)
# > cacheSolve(cmat)

# Function 'makeCacheMatrix()':
# Creates a special "vector", which is really a list containing a function to:
# 1. set the matrix to be inverted
# 2. get the matrix to be inverted
# 3. set the inverted matrix
# 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL

    # set the matrix to be inverted, maintang the state across function invocations
    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    # get the matrix to be inverted
    get <- function() x

    # set the inverted matrix
    setSolve <- function(solve) s <<- solve

    # get the inverted matrix
    getSolve <- function() s

    # return our list containing the 4 functions
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)

}


# Function 'cacheSolve()':
# Returns a matrix that is the inverse of the proessed matrix (see 'makeCacheMatrix()')
# If the inverse has been cached, nothing is computed and cache is returned

cacheSolve <- function(x, ...) {

    # any cache exists?
    s <- x$getSolve()

    #... if yes, return the cache
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    # otherwise, get the matrix and process it
    data <- x$get()
    s <- solve(data, ...)

    # store processed matrix for caching, and deliver it
    x$setSolve(s)
    s

}
