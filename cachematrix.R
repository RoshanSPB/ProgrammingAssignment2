## following two functions are used to compute inverse of a rectangle matrix and 
## cache it for later use

## The first function 'makeCacheMatrix' creates a special matrix which returns 
## a list of functions, its return following items in a list.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x <<- y
    m <<- NULL  
  }
  get <-function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function 'cacheSolve' computes the inverse of the special matrix
## returned from the makeCacheMatrix. Prior to calculate the inverse it check
## the already calculated inverse of the matrix from the cashe.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

