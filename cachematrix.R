## This file contains two functions whose overall purpose is to store a matrix in an object, 
## compute the inverse of that matrix and cache it in the object for future reference.

## This funcion takes a matrix "x" as the argument and returns an object 
#  containing a list of four functions and the variables x(original matrix) and x1(inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
  x1 <- NULL
  set <- function(y) {
    x <<- y
    x1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x1 <<- inverse
  getinverse <- function() x1
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cachesolve takes the object returned by makeCacheMatrix as the argument.
## If the value of the inverse matrix(x1) is NULL cacheSolve will calculate 
## the inverse matrix and cache it in the makeCacheMatrix object using the
## setinverse() function. If the getinverse() function returns a matrix then 
## the function simply returns the value of x1 stored in the makeCachematrix object.

cacheSolve <- function(x, ...) {
  x1 <- x$getinverse()
  if(!is.null(x1)) {
    message("getting cached matrix")
    return(x1)
  }
  data <- x$get()
  x1 <- solve(data, ...)
  x$setinverse(x1)

}
