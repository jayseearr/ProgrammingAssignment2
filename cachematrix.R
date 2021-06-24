## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a matrix that has a cached inverse. This is useful
## for large matrices, where computing the inverse (using the 'solve' function)
## is time consuming. A cached matrix comptues the inverse the first time it is
## requested, and returns the pre-computed cached value on all subsequent 
## requests.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(Y) {
      X <<- Y
      inv <<- NULL
    }
    get <- function() X
    setinv <- function(I) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the cacheMatrix. The first call to 
## cacheSolve results in computing the inverse, and all subsequent calls return
## a cached value of the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- X$getinv()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    M <- X$get()
    inv <- solve(M,...)
    X$setinv(inv)
    inv
}
