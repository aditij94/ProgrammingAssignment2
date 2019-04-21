## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x,y) {
  a <- matrix(x,nrow=y,ncol=y)
  i <- NULL
  set <- function(x1,y1){
    a <<- matrix(x1,nrow=y1,ncol=y1)
    i <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## Write a short comment describing this function

cacheSolve <- function(x){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}