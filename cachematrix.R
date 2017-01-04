## Following the technique implemented in the cachemean and cacheSolve functions
## in the introduction of this assighment, The following two functions compute 
## the inverse of a square matrix using the caching technique applied in the above
## mean calculation. In order to compute the inverse of a matrix, we have two functions  
## makeCacheMatrix which creates a special matrix object that can cache its inverse
## and cacheSolve which computes the inverse of the matrix object created and returned 
## by the function makeCacheMatrix

## makeCacheMarix is a function that takes a square invertible matrix as an
## argument. This is an assumption made.
## We start by initializing an inverse matrix. Then creating a cach once the inverse is
## calculated or changed in set function, The function get gets the inverse matrix. 
## The setInverse function implements the solve function in R to compute the inverse
## of the matrix object. The function getInverse gets the inverse matrix. Then we 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function that gets the inverse of the matrix if it is cached by 
## returning it if the is.null(inv) is not true, which means that the inverse does
## exist. Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
