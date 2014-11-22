## The two functions below use the <<- operator to assign a value
## to an object in an environment that is different than the 
## current environment. The two functions below are used to create
## a special matrix object that stores a matrix and caches its
## inverse

## The first function creates a matrix object that can cache or store its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}
## This function returns the inverse of the matrix created by the makeCacheMatrix above.
## If the inverse has already been calculated (And there has been no change to the matrix),
## Then the cacheSolve will return the inverse from the cache.

cacheSolve <- function(x, ...) {
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
## Return a matrix that is the inverse of 'x'
