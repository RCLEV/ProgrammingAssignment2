## This pair of functions cache the inverse of a matrix 
## The makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse

## Caching the inverse of a matrix is beneficial, so that it 
## doesn't need to be doing costly computations repeatedly

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() { 
            x
      }
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function()inv
      list(set=set, get=get, 
            setInverse=setInverse, 
            getInverse=getInverse)
}

## The cacheSolve function computes and retrieves the inverse 
## of the special "matrix" returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat_data <- x$get()
      inv <- solve(mat_data, ...)
      x$setInverse(inv)
      inv
}
