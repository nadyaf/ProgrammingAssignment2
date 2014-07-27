## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize an inverse matrix
  invm <- NULL
  
  ## sets the matrix
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  ## returns the matrix
  get <- function() x
  
  ## sets inverse of a matrix
  setinverse <- function(inv) {
    invm <<- inv
  }
  
  ## returns inverse of a matrix
  getinverse <- function() invm
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
##    then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invm <- x$getinverse()
  
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  
  ## if the inverse matrix wasn't retrieed from the cache, calculate it 
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse of the matrix
  invm <- solve(data, ...)
  
  ## set the inverse
  x$setinverse(invm)
  
  ## return the inverse matrix
  invm
}
