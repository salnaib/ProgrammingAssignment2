## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Defines the makeCacheMatrix object, which stores the value of the matrix x, passed
## into the initialization function, as well as the Solve of x, and the 4 functions
## used to re-set and access both x and it's Solve, allowing the caller to get the Solve
## without having to recalculate it more than once.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize s, which will be the result of the solve on x
  s <- NULL
  ## The function set of makeCacheMatrix to re-set x
  set <- function(y) {
    ## sets the x value of makeCacheMatrix to the parameter y
    x <<- y
    ## Nulls out the g value of makeCacheMatrix
    s <<- NULL
  }
  ## get function returns the x value of makeCacheMatrix
  get <- function() x
  ## setSolve function sets the s value of makeCacheMatrix to the solve parameter
  setSolve <- function(solve) s <<- solve
  ## getSolve function returns the s value of makeCacheMatrix
  getSolve <- function() s
  ## Initializing the makeCacheMatrix object returns it with the 4 functions defined
  ## above. Naming the functions allows the caller to use the $ sign to access them.
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function
## The cacheSolve takes in an object of type makeCacheMatrix and attempts to 
## get the Solve of it's x value from it's "cache" if it exists 
## (has been previously calculated and stored) in it. Otherwise it calculaes it and
## stores it in the makeCacheMatrix object to save the recalculation next time
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempt to get the Solve from the "cache"ed s variable in the makeCacheMatrix object
  ## passed in (x), which would be there if this function was previously calculated
  s <- x$getSolve()
  ## Make sure that the result is not null (and so was previously calculated)
  if (!is.null(s)) {
    ## Message to the caller informing them that the data returned is cached
    message("getting cached data")
    ## return the cached Solve
    return(s)
  }
  ## get the matrix from the makeCacheMatrix object and store it in data
  data <- x$get()
  ## gets the solve of data (the matrix stored in makeCacheMatrix) and stores it in s
  s <- solve(data, ...)
  ## sets the solve s of the makeCacheMatrix object to the s value set in the previous line
  x$setSolve(s)
  ## returns the value of s set 2 lines prior
  s
}
