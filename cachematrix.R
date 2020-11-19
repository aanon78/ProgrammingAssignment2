#The function creates a matrix object that caches its inverse. The user will submit a matrix object on "y"

makeCacheMatrix <- function(x = matrix()) {
    invs<-NULL
    set<- function(y){
      sample<<-y
      invs<<- NULL
    }
    get<-function()(y)
    setInverse<-function(inverse)(invs<<-inverse)
    getInverse<-function()(invs)
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
###############################################################################################################

#The function computes the inverse created by the formula above.
#If the inverse has already been calculates, it should return the inverse of the cache.

cacheSolve<-function(y, ...){
  invs<-x$getInverse()
  if(!is.null(invs)){
    message("getting cached data")
    return(invs)
  }
  mat<-y$get()
  invs<-solve(mat,...)
  y$setInverse(invs)
  invs
}
