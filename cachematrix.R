## Put comments here that give an overall description of what your
## functions do

## This is an object which can "cache" its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse of the matrix
  inv <- NULL
  
  ## Sets the m matrix
  set <- function(matrix)
  {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Gets the m matrix
  get <- function()
  {
    m
  }
  
  ## Sets the inverse of the m matrix
  setInv <- function(inverse)
  {
    inv <<- inverse
  }
  
  ## Gets the inverse of the m matrix
  getInv <- function()
  {
    inv
  }
  
  ## Returns the list of the available methods
  list(set = set, get = get, setInv = setInv, getInv = getInt)
}


## This is a function to compute the inverse of a matrix using
## the makeCacheMatrix. Here, if the inverse was calculated prev
## iously, then we return the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    
    ## Is the inverse calculated?
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    ## Solve gives the inverse. %*% performs the matrix mult
    m <- solve(data) %*% data
    
    ## Save the inverse inside the object
    
    x$setInv(m)
    
    ## Return the matrix object
    m    
}
