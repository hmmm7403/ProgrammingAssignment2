## This program contains 2 functions that cache the inverse of a matrix.

## Matrix inversion is usually a costly computation.  When the contents of a mtrix 
## are not changing, it is desirable to cache the inverse of a matrix to avoid computing 
## matrix inversion repeatedly. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y){
    x<<-y
    invx<<-NULL
  }
  get<-function() x
  setinv<-function(inv) invx <<-inv
  getinv<-function() invx
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## cacheSolve computes the inverse of a matrix returned by makeCacheMatrix.  If the inverse
## has already been calculated, cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx<-x$getinv()
  if(!is.null(invx)){
    message('getting cached data...')
    return(invx)
  }
  data<-x$get()
  invx<-solve(data,...)
  x$setinv(invx)
  invx
  
}
