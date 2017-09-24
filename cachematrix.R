##  a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
## which is really a list containing 4 functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  iv.matrix <- NULL
  set <- function(y) {
    x <<- y
    iv.matrix <<- NULL
  }
  get <- function() x
  setinver <- function(inver.m) iv.matrix <<- inver.m
  getinver <- function() iv.matrix
  list(set = set,  get = get,
       setinver = setinver,
       getinver = getinver)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inversed matrix has already been calculated. 
## If so, it gets the the inversed matrix from the cache 
## and skips the computation. Otherwise, it calculates 
## the inversed matrix of the data and sets the value 
## of the inversed matrix in the cache via the setinver function.


cacheSolve <- function(x, ...) {
  
  iv.matrix <- x$getinver()
  if(!is.null(iv.matrix)) {
    message("getting cached inverted matrix")
    return(iv.matrix)
  }
  data <- x$get()
  iv.matrix <- solve(data, ...)
  x$setinver(iv.matrix)
  iv.matrix
}
