## write a pair of functions that cache the inverse of a matrix

## Creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x1 = matrix()) {
  
  a <- NULL
  
  set <- function(ma) {
    x1 <<- ma
    a <<- NULL
  }
  
  get <- function() x1
  
  setInverse <- function(inv) a <<- inv
  
  getInverse <- function() a
  
  return(list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse))
  
}


## Compute the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ##  Compute the inverse of the special matrix returned
  x1 <- x$getInverse()
  
  if(!is.null(x1)) {
    message('getting cached data')
    return(x1)
  }
  
  data <- x$get()
  x1 <- solve(data) %*% data
  x$setInverse(x1)
  
  return(x1)
  
}
