## Put comments here that give an overall description of what your
## functions do

##Overall Design 
## Cachematrix contains two functions one is makechchematrix and other one cachesove

##The key concept to understand in makeCachematrix() is that it builds a set of functions and 
##returns the functions set, get , setinverse and getinverse  within a list
##It also contains data elements inv and x which are of type matrix 
##Testing samples
##> #2elements matrix rbind(c(1,2),c(3,4))
##> f2 <-  makeCachematrix(rbind(c(1,2),c(3,4)))
##> Cachesolve(f2)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> Cachesolve(f2)
##getting cached inverse
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> Cachesolve(f2)
##getting cached inverse
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> #3elements matrix rbind(c(1,2,1),c(3,4,2),c(1,0,1))
##> f3 <-  makeCachematrix(rbind(c(1,2,1),c(3,4,2),c(1,0,1)))
##> Cachesolve(f3)
##[,1]          [,2] [,3]
##[1,] -2.0  1.000000e+00  0.0
##[2,]  0.5 -4.163336e-17 -0.5
##[3,]  2.0 -1.000000e+00  1.0
##> Cachesolve(f3)
##getting cached inverse
##[,1]          [,2] [,3]
##[1,] -2.0  1.000000e+00  0.0
##[2,]  0.5 -4.163336e-17 -0.5
##[3,]  2.0 -1.000000e+00  1.0
##
makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y        #Lexxical scoping , writes to the evironment
    inv <<- NULL
  }
  get <- function() x
  setinverse  <- function(inverse) inv <<- inverse #Lexxical scoping , writes to the evironment
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse  = getinverse)
}


## Return a matrix that is the inverse of 'x'
##cachesolve takes the oject returned by the fuction makeCachematrix and returns inverse if exists 
##otherwise creates the inverse using the built in function solve and calls the setinverse to write to the environement. 
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
