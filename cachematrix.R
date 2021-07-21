## There are four usage of makeCacheMatrix:
## First, it can set the value of the matrix
## Second, it can get the value of the matrix
## Third, it can set the inverse matrix
## Finally, it can get the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  get<-function()
  {
    m
  }
  
  setinverse<-function(inverse)
  {
    m<<-inverse
  }
  
  getinverse<-function()
  {
    m
  }
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## Function cacheSolve can first identify whether there are cached data,
## if it's already in, then return cached values
## otherxise it can calculate the inverse matrix and add it in.

cacheSolve <- function(x, ...)
{
  m<-x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
