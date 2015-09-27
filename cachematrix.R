## This is a R function which is able to cache potentially time-consuming matrix
## inversion computations. If the contents of a matrix are not changing, it may
## compute the mean, especially if it has to be computed repeatedly, it can be 
## looked up in the cache rather than recomputed. 

## function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse. It contains a list of four functions.

makeCacheMatrix<-function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)
  m<<-inverse
  getinverse<-function() m
  list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}
## Function "cacheSolve" calculates the inversion of the special "matrix"
## created with the above function. However, it first checks to see if the 
## matrix has already been calculated. If so, it gets the inversion from the
## cache and skips the computation. Otherwise, it calculates the inversion of
## the data and sets the value of the inversion in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
