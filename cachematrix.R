## Following are two functions that can cache the inverse of a matrix

## This will create a specila matrix object that can cache an inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse
  j <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## set the inverse of the matrix
  setmatInverse <- function(inverse) {
    j <<- inverse
  }
  
  ## get the inverse of the matrix
  getmatInverse <- function() {
  
    j
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setmatInverse = setmatInverse,
       getmatInverse = getmatInverse)
}




## This will compute the inverse of the special matrix returned by "makeCacheMatrix"
##"cachesolve" should retrieve the inverse from the cache if the inverse is already calculated and if the matrix is the same without changes.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatInverse()
  
  ## if already set, return the inverse 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Using matrix multiplication to calculate the inverse 
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setmatInverse(m)
  
  ## Return the matrix
  m
}
  
  
