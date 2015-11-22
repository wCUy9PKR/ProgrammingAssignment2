## Matrix with inverse caching
##
## Matrix inversion is a lengthy operation.  By caching the matrix inverse along
## with the matrix, we may be able to speed up calculations which repeated require
## the inverse of a given matrix.


## Implements a matrix as a list of setters and getters
## set/get matrix
## set/get matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  xi <-  NULL
  set <- function(y) {
    x <<- y    
  }
  get <- function() x
  setinv <- function(xinv) xi <<- xinv
  getinv <- function() xi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calculates inverse of matrix as returned by makeCacheMatrix
## and caches matrix inverse along with matrix.
## Note: we assume input matrix is invertible
## Note: inverse is not re-calculated if inverse is already cached
cacheSolve <- function(x, ...) {
  xi <- x$getinv()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- solve(data, ...)
  x$setinv(xi)
  xi
}