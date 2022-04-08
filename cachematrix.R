## This script will be used to calculate the inverse matrix in a given data ##
## To execute this task two different functions will be needed:
##   1. Generates the an "empty" matrix that can store the inverse matrix result
##   2. Calculates the inverse matrix from the data or recover it if it was already calculated.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL     # creates a empty matrix that will be replaced later
  set <- function(y) {
    x <<- y    
    iMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) iMatrix <<- solve  #set the value of the inverse matrix
  getInverse <- function() iMatrix        #get the value from the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)   # create a list with all objects
}


## Write a short comment describing this function

cacheInverse <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getInverse()    #checks if the inverse matrix is present
  if(!is.null(iMatrix)) {
    message("getting cached data")  #If it is present, it will return the written message followed by the value
    return(iMatrix)
  }
  data <- x$get()  # If it is NOT present, it will take the matrix...
  iMatrix <- solve(data, ...)  # ...and apply the function "solve" to return the inverse matrix. 
  x$setInverse(iMatrix) # Here the recently created inverse matrix will be added to the data
  iMatrix
}

