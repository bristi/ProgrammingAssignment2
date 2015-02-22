## In order to allow caching the inverse of a matrix two functions are 
## written; One providing a data structure for a matrix containing it's
## own inverse and one providing functionality for using this data structure.


## Create a special matrix object that allows storing the value of it's
## own inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Default value for inverse
  i <- NULL
  
  # Initialize a new object from a given matrix y
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Return matrix representation
  get <- function() x
  # Get and set the inverse for this matrix
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  # Data structure internals
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of a matrix defined using the special matrix object.
## If the inverse of the matrix has not yet been calculated then do this
## now and cache it in the object.
cacheSolve <- function(x, ...) {
  # Get the inverse of the matrix object
  i <- x$getinverse()
  # If it is already calculated then return this value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Get the matrix representation and calculate the inverse
  data <- x$get()
  i <- solve(data, ...)
  # Cache it in the matrix object and return the calculated inverse
  x$setinverse(i)
  i
}




