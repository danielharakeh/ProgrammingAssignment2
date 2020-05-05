## In this function I create a matrix object that can cache its inverse.
## I use the <<- operator to assign the value in the environment different from the current.
## After creating the matrix, I include functions to set & get values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getmInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the matrix created in the first function.
## If already calculated it gets the value from the cache and skips the calculation.
## Otherise it calculates the inverse and sets the value in the cache using the set function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
