## As a part of Programming Assignment 2, the two functions below calculate and cache 
## the inverse of a matrix since it can be a costly computation. 


## This function creates a special 'matrix' object whose inverse is later calculated.
makeCacheMatrix <- function(x = matrix()) {
  
  m<- NULL
  set<-function(y){
    # set matrix 'x' to argument 'y'
    x<<-y
    m<<-NULL
  }
  
  get<-function() x #returns the matrix x
  setinv<- function(inverse) m<<-inverse # store inverse value
  getinv<- function() m #returns inverse value m
  
  list(set=set, get=get, setinv = setinv, getinv=getinv)
  
}


## This function calculates the inverse of the object myCacheMatrix returned by the above function.
## If the matrix value has already been calculated (unless the matrix is new), the value is fetched 
## from the cache and returned.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv() #try to get inverse from cache
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #if cache is empty...
  data<-x$get()
  m<-solve(data) #calculate inverse here
  x$setinv(m) #store in cache
  m 
  
}
