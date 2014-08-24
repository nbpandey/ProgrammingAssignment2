### Functions for Caching the Inverse of a Matrix


## This function creates a matrix object which cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }

  get <- function() x
  
  # set the inverse of the matrix using solve function in R
  setinversem <- function(solve) m <<- solve
  
  # get the inverse matrix
  getinversem <- function() m
  
  # output the above functions as list
  list(set=set, get=get, setinversem=setinversem, getinversem=getinversem)
} 



## This function checks in Cache before doing calculation for inverse of matrix.
cacheSolve <- function(x, ...) {
  m <- x$getinversem()
  
  # if m already exists, return m from cache 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # compute the inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinversem(m)
  m
}
