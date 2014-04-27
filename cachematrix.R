## These functions will cache the inverse of a matrix

##  The makeCacheMatrix function creates a special "matrix" object 
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL

  ##  set the values in the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##  get the values in the matrix
  get <- function() x
  
  ##  set the values of the matrix inversion
  setinverse <- function(solve) m <<- solve
  
  ##  get the values of the matrix inversion
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix" created 
## with the makeCacheMatrix function. It first checks if the inverse has been 
## computed. If so, it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the 
## values of the matrix inversion in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## Get the inverse of matrix 'x'
  m <- x$getinverse()
  
  ## Check if matrix inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Calculate inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  
  ## Cache the matrix inversion
  x$setinverse(m)
  m
}


