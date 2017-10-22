## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##Function to modify the matrix obj in the list returned by makeCacheMatrix
  ##so that the list obj can be modified on the fly without creating new lists for new matrices
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##function to get the cached matrix
  get <- function() x
  ##function to set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  ##function to get cached inverse of the matrix
  getinverse <- function() i
  #returning a list with matrix and the inverse data
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #start by getting the inverse from the list obj
  i <- x$getinverse()
  #check if the inverse already exsits for the input matrix obj
  if(!is.null(i)) {
    message("getting cached inverse")
  #return the cached inverse of the input matrix obj
    return(i)
  }
  #if the inverse is not present get the matrix from the list obj
  data <- x$get()
  #solve for the inverse of the matrix using the solve(x) function
  i <- solve(data, ...)
  #cache the inverse in the list obj using its setinverse functions
  x$setinverse(i)
  #Return the value of the inverse calculated
  i
}
