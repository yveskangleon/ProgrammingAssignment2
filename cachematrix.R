# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute 
# it repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not 
# changed), then the cachesolve should retrieve the inverse from the 
# cache.  Computing the inverse of a square matrix can be done with 
# the solve function in R. For example, if X is a square invertible 
# matrix, then solve(X) returns its inverse.  For this assignment, 
# assume that the matrix supplied is always invertible.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. It first checks if the
# inverse has already been computed. If so, it gets the result and 
# skips the computation otherwise, it computes the inverse, sets the 
# value in the cache via setinverse function.  This function assumes 
# that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data.")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
}
