## x - input, for the purposes of this exercise, assumed to be an inversible square matrix
## output - a list of 4 functions
  ## $get - outputs x
  ## $set - re-assigns value of x and caches its inverse, input is x 
  ## $getinv - outputs the inverse of x
  ## $setinv re-assigns value of x and caches its inverse, input is inverse of x
## adapted from the makeVector function

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- solve(y)
  }
  get <- function() x
  setinv <- function(y_inv) {
    x_inv<<-y_inv
    x<<-solve(y_inv)
  }
  getinv <- function() solve(x)
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## x - input, which is the list output from the makeCacheMatrix function
## output - the inverse of the matrix passed through the makeCacheMatrix function
  ## if a matrix was previously run through the makeCacheMatrix, cache will be the output
  ## if there is no cached matrix, solve function is called


cacheSolve <- function(x, ...) { 
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get() ##not entirely understanding this part, but would there be a scenario where x$get is assigned, but x$getinv is not?
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}