
# Below is a pair of functions that cache the inverse of a matrix.
# This is done because matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly.


# The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a function to:
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(x2) {
    x <<- x2
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}