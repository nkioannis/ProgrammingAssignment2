##The following function creates a special "vector" which is really a list
##containing a function to: set and get the value of a matrix
##set and get the value of the inverse of a matrix
makeCacheMatrix<-function(x = numeric()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) m<<- solve
  getinverse<-function()m
  list(set= set, get = get, setinverse= setinverse, getinverse= getinverse)
}
##The following function calculates the inverse of the given matrix using
##the first function. If the inverse have been calculated
## (getinverse is not null) then returnes the inverse from the cache
##else, the function calculates the inverse and save it to the cache
cacheSolve<-function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}