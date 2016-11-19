## Following functions will inverse a matrix or pull a inverted matrix from cache.
## makeCacheMatrix will create a object that will store/cache the inverse of a given 'invertable'matrix
## With alexical scoping perspective, some variable below are defined in the environment that is not where mainfunction is defined. 
## Function 'set' will return matrix 'x' stored in the main function.
## Function 'get' changes matrix 'x' stored in main function.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
       x <<- y
     m <<- NULL
  }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get, setinverse = setinverse, 
        getinverse = getinverse)
}
           
## cachesolve calculates the inverse of a invertable matrix pulling it from cache.
## If the inverse of matrix is already calculated, then this function should 'out' the result from cache
## If the inverese is not yet calculated, then this function should store the inverse matrix in 'm'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
     message("getting cached data")
      return(m)
 }
   data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
     m
}
           
