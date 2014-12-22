# The purpose of the following functions are to cache 
# the inverse of a matrix, rather than having to compute 
# it repeatedly which is not as efficient. 

# The makeCacheMatrix function, I hope, will be able to set and 
# also get the value of a matrix and then set and get the value 
# of the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function should return the inverse 
## of the matrix. The function will assume that the matrix is 
## always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("retrieving cache")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  
  x$setinverse(m)
  m
}