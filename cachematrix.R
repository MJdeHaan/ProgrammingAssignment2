## Function which allows the caching of a matrix inverse, as well
## as the corresponding function which return the inverse of the 
## matrix

## Function which allows the caching of an inverted matrix using 
## R lists. Input should be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Create empty variable for the inverse

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  # return functions to use the object
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function which return the inverse of a matrix that is made using 
## the makeCacheMatrix function. If the inverse is not yet calculated,
## It is calculated and stored in the cache for later use

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting already inverted matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
}






