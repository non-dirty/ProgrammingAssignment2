## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("using cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  
  return(inverse)
}
