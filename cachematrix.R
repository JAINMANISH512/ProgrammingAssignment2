## This function creates a special "matrix" object that can cache its inverse(inv).
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  
  ## This function sets the value of special "matrix" object.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  ## This function returns special "matrix" object.
  get <- function()  {
    x
  }
  
  
  ##This function sets the inverse(inv) of special "matrix" object.
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  
  ##This function returns the inverse(inv) of special "matrix" object. 
  getInverse <- function() {
    inv
  }
  
  
  ##Returns a list containing a reference to each function inside makeCacheMatrix:
  ##namely set(), get(), setInverse, getInverse .
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

##If the inverse has already been calculated (and the matrix has not changed),  
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  ##If the inverse of the matrix is already cached.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  ## Calculate and set the inverse of matrix if it is not cached already.
  data <- x$get()
  
  ##Function solve(x),returns inverse of matrix 'x'
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
