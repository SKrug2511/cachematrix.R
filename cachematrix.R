
makeCacheMatrix <- function(x = matrix()) {   # cached matrix which has to be inverted
 
  inv <- NULL
  get <- function() x  #get/set the value of the matrix for inverting
  set <- function(y) {
  inv <<- NULL
  x <<- y
  }
  
  getinverse <- function() inv
  setinverse <- function(inv) inv <<- inverse

  list(get=get, getinverse=getinverse, setinverse=setinverse)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
  message("inverse is cached")  ## check if the inverse has been calculated
  return(inv)
  }
  
  m <- x$get()           # get the inverse matrix
  inv <- solve(m, ...)
  x$setinverse(inv)
  
  return(inv)               # solution 
}

data <- matrix(c(1, 3, 4, 1, 3, 4, 4, 3, 0), nrow=3, ncol=3)
