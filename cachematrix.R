## cachematrix.R contains functions used in the creation of cacheMatrix 
## objects. A cacheMatrix has the functionality of a matrix object, but stores
## the inverse of the matrix (obtained via the 'solve' function) in a
## persistent variable once it has been solved for once. 
##
## Example usage:
## N <- 100
## X <- matrix(rnorm(N*N),N,N)   # create a standard NxN matrix with N*N random elements.
## Y <- makeCacheMatrix(X)       # create a cacheMatrix identical to the matrix X.  
## Yi <- cacheSolve(Y)           # returns the inverse of Y (analogous to solve(X)).
##
## Y$get()      # returns the NxN matrix
## Y$getinv()   # returns NULL if the inverse has not been solved for yet, and the inverse of X otherwise (equal to solve(X))


## makeCacheMatrix returns a matrix that has a cached inverse. This is useful
## for large matrices, where computing the inverse (using the 'solve' function)
## is time consuming. A cached matrix comptues the inverse the first time it is
## requested, and returns the pre-computed cached value on all subsequent 
## requests.

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
