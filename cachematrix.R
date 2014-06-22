## The following functions create a special "matrix" that can 
## cache its inverse, and solve for the inverse of any invertible
## matrix if not found in the cache.



## This function creates special a "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m 
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
    m <- x$getinv()
    if(!is.null(m)){
      message("retrieving cached data")
      return(m)
    }
  ## Return solved matrix that is the inverse of 'x'
    mat <- x$get()
    m <- solve(mat,...)
    x$setinv(m)
    m
}
