## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a matrix and put it in "cache"
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) inv <<- solve 
  getinvert <- function() inv
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## Write a short comment describing this function
## Calculates the invert of a matrix using the cache matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  
  #if the cachematrix exist, the function gets the cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Let's create inverted matrix in case
  # there's no cached matrix available.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvert(inv)
  inv
  
}
