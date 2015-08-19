## Put comments here that give an overall description of what your
## functions do

##vich

## makeCacheMatrix should make be able to cache an inverse of a matrix x, that doesn't have to be square

makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set <-function (v) {
    x<<-y
    invx<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) invx<<-inverse
  getinv<-function() invx
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve should 1. check for inverse of a matrix and if not it doesn't find it should 2. make inverse of matrix

cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if (!is.null(invx)){
    message("getting cached data")
    return (invx)
  }
  data <-x$get()
  invx<-solve(data,...)
  x$setinv(invx)
  invx
  
  ## Return a matrix that is the inverse of 'x'
}