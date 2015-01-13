# This function creates a special matrix object that can caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # initializing the inverse matrix
  i <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get the original matrix
  get <- function() x
  
  # Setter for the inverse
  setsolve <- function(solve) i <<- solve
  
  # Getter for the inverse
  getsolve <- function() i
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  
  # If the inverse is already calculated then the below logic returns the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Computing the inverse of the matrix
  data <- x$get()
  i <- solve(data, ...)
  
  # Cache the inverse and returning the inverse matrix
  x$setsolve(i)
  i
}
