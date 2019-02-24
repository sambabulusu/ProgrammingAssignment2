## Put comments here that give an overall description of what your
## functions do


##
makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse  <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse  = getinverse)
}

## Return a matrix that is the inverse of 'x'
Cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
