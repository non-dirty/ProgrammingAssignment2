## These functions provide methods for obtaining the inverse of a matrix and cache the result
## for improved performance in subsequent calls.

## This function creates a list which contains functions for getting and setting
## a matrix as well as getting and setting the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  get <- function(){
    x
  }
  
  set <- function(aMatrix){
    x <<- aMatrix
    inverse <<- NULL
  }
  
  getInverse <- function(){
    inverse
  }
  
  setInverse <- function(anInverse){
    inverse <<- anInverse
  }
  
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}



## Return a matrix that is the inverse of 'x'
## If a cached result is available, uses it, otherwise cached the computed result.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("using cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  
  inverse
}
