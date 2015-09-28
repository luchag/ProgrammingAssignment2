## Programming Assignment 2

## Matrix inversion can be difficult to compute and sometimes it is needed to
## cache the inverse of a matrix rather than compute it. Below are two functions
## that can be used to create an object that stores a matrix and caches the inverse.

## Write a short comment describing this function: This first function 
## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function: this second function 
## computes the inverse of the special "matrix" created by function above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}