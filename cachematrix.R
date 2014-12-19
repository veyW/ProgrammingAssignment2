## These functions will work together to cache the inverse of a square matrix  
## in order to reduce the computational load on the system


## first function creates a special "matrix" object with several get/set functions
makeCacheMatrix <- function(X = matrix()) {
  
  i <- NULL
  set <- function(Y) {
    X <<- Y
    i <<- NULL
  }
  get <- function() X
  setinv <- function(inv_X) i <<- inv_X
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## second function calculates inverse of the matrix or takes it from already computed value
cacheSolve <- function(X, ...) {
        
  inverse <- X$getinv()
  # check if inverse is already calculated
  if(!is.null(inverse)) {
    message("Getting cached data...")
    inverse
  } 
  else {
  data <- X$get()
  inverse <- solve(data, ...)
  X$setinv(inverse)
  inverse}
}

