## This R script creates a special invertible matrix which has functions 
##  defined to create, set, get and invert the matrix.
## The matrix inversion is implemented such that once computed, it is cached and
##  subsequent calls to the invert the matrix retrieves the inverse from the cache
##  if the matrix hasnt been updated, otherwise the inverse is recomputed.

## The makeCacheMatrix function creates a special invertible matrix
##  with setters and getters
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## setter and getter for the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    ## setter and getter for the inverse of the matrix
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    ## encapsulation as a list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the input matrix x created using
##  the makeCacheMatrix function defined above.
## cacheSolve first tries to fetch the inverse from the cache and computes it 
##  on the fly only if it cannot find the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
      message("Fetched from cache")
      return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
