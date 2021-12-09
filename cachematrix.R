## cachematrix.R contains functions used in the creation of cacheMatrix 
## objects. A cacheMatrix is really a list of functions that do the following:
##   1. Set the value of the matrix
##   2. Get the value of the matrix
##   3. Set the inverse of the matrix 
##   4. Get the inverse of the matrix (from cached value, if possible)
##
## After the inverse has been computed once (by calling the cacheSolve function), 
## the inverse is stored in a persistent variable so that it doesn't have to be
## computed each time cacheSolve is called. Note that the inverse should only
## be accessed from the cacheSolve(Y) function, not from Y$getinv(); this avoids 
## a return value of NULL if getinv is called before cacheSolve.
##
## Example usage:
## N <- 100
## X <- matrix(rnorm(N*N),N,N)   # create a standard NxN matrix with N*N random elements.
## Y <- makeCacheMatrix(X)       # create a cacheMatrix identical to the matrix X.  
## Yi <- cacheSolve(Y)           # returns the inverse of Y (analogous to solve(X)).
##
## XX <- Y$get()                 # returns the NxN matrix (equal to X)


## makeCacheMatrix returns a "matrix" (a list of 4 functions) that can contain 
## a cached inverse. This is useful for storing large matrices, where computing 
## the inverse (using the 'solve' function) is time consuming. A cached matrix 
## comptues the inverse the first time it is requested, and returns the pre-computed 
## cached value on all subsequent requests.

makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y) {
      X <<- Y
      inv <<- NULL
    }
    get <- function() X
    setinv <- function(I) inv <<- I
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the cacheMatrix. The first call to 
## cacheSolve results in computing the inverse, and all subsequent calls return
## a cached value of the inverse.

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'. Note that the returned value
    ## is of class "matrix", not a cachematrix.
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
