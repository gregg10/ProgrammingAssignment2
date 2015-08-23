## These functions two allow to calculate the inverse of a matrix 
## and cache the result so it can be used later


## This function creates a list that contains functions
##that allow to create a special matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)  inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the special matrix returned by
## makeCachematrix above and cheks if the inverse is not in cache. If yes,
## instead of calculate de inverse, it returs the value in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
