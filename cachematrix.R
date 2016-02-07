## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # in s is stored the value of the inverse
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x # this command will return the input matrix x
  setsolve <- function(solve) s <<- solve # this command will set inverse of x
  getsolve <- function() s # this command will return inverse of x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  #this function return a list with created objects so as we can use them in other functions
  #such ad cacheSolve
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()# get the inverse of x and it will be null if not calculated before
  if(!is.null(s)) { # if we already have the inverse, then return it
    message("getting cached data")
    return(s)
  } #otherwise, calculate the inverse of matrix x and return it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}