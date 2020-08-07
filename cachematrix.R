## Put comments here that give an overall description of what your
## functions do
## These functions ("makeCacheMatrix" and "cacheSolve") cache 
## the inverse of a matrix
## Write a short comment describing this function
## This function (makeCacheMatrix)is responsible for creating special "matrix" 
## object which cache its inverse for the input (invertible square matrix)

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## While cacheSolve function helps to compute the inverse of the special "matrix" 
## which is returned by the previous function (makeCacheMatrix) 
## cacheSolve function will retrieve the inverse from the cache in the case 
## when the inverse was calculated and the matrix did not changed

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
