## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix creates a cache version of the input 
## matrix and its inverse, utilising the lexical scooping of R.

makeCacheMatrix <- function(x = matrix()) {
  ## initialising the inverse
  inv <- NULL
  ## a new function for setting the values of variable with <<-
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## a new function to take x as a free parameter
  get <- function() x
  ## another new function to create cache when the inverse is 
  ## solved 
  setinv <- function(solve) inv <<- solve
  ## a new function to take the inverse as a free parameter
  getinv <- function() inv
  ## a list of the cache as the output
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

## The cacheSolve function computes the inverse of ## the cached 
## matrix created by the makeCacheMatrix ## function. The inverse 
## is computed through the solve function when no cache is found,
## otherwise the cached inverse is recalled instead.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## read the cached inverse from the makeCacheMatrix function
  inv <- x$getinv()
  ## get the inverse if its cache versio is available
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## read the matrix from makeCacheMatrix as data
  data <- x$get()
  ## compute inverse of data
  inv <- solve(data, ...)
  ## created cache inverse for future usage
  x$setinv(inv)
  ## print the inverse
  inv
}
