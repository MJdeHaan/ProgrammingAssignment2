## Function which allows the caching of a matrix inverse, as well
## as the corresponding function which return the inverse of the 
## matrix

## Function which allows the caching of an inverted matrix using 
## R lists. Input should be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Create empty variable for the inverse

  # Set function for updating the data
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Resetting the cache
  }
  get <- function() return(x)  # get function for data
  
  # Set and get functions for the inverse for caching
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() return(inv)
  
  # return functions to use the object
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function which return the inverse of a matrix that is made using 
## the makeCacheMatrix function. If the inverse is not yet calculated,
## It is calculated and stored in the cache for later use

cacheSolve <- function(x, ...) {
  # If cached available, return that
  if(!is.null(x$getInv())) {  
    message("getting already inverted matrix")
    return(x$getInv())
  }
  
  # Need to calculate the inverse and cache it
  x$setInv(solve(x$get(), ...))
  return(x$getInv())
}
