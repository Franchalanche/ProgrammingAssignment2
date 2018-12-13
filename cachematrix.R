## These functions create a matrix and helps compute its inverse with a cache, m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Defines function that sets the value of the matrix, and also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    # Sets the value of x
    m <<- NULL # Clear the cache
  }
  # This is the function to get the value of the matrix
  get <- function() x
  # This defines the function to set the inverse. It is only used by getinverse() when
  # there is no cached inverse.  Note that the cache is listed after the function..
  setInverse <- function(inverse) m <<- inverse
  # Defines the function to get the inverse
  getInverse <- function() m
  
  # Returns a list w/ the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This section returns the inverse matrix.

cacheSolve <- function(x, ...) {
  # Get the current state of the inverse, see if it has been computed yet
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    # If the value isn't null, it means that inverse computation is complete		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If not, get the matrix itself
  data <- x$get()
  
  # Find inverse
  inv <- solve(data, ...)
  
  # Caches this result in the object itself!
  x$setinverse(inv)
  
  # Returns new result
  inv    
}