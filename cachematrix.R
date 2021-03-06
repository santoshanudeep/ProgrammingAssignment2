## Put comments here that give an overall description of what your
## functions do


## This funtion takes a matrix as an argument and returns a list obj with the following functions
##setMatrix - function to cache the matrix obj given.
##getMatrix - function to get the cached matrix.
##setinverse - function to cache the inverse of the matrix.
##getinverse - function to get cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = setMatrix, get = getMatrix,
       setinverse = setInverse,
       getinverse = getInverse)
}


##This function takes the special matrix object generated by the makeCacheMatrix and
##checks if the inverse has been cached.
##If the inverse has been cached(and the matrix remains unchanged) it returns the inverse
##Else it calculates the inverse of the matrix and caches it in the special matrix obj and
##returns the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
