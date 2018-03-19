## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
## Set function        
  set<-function(y){
    x<<-y
    invMatrix <<-NULL
  }  
## Get Function  
  get <- function() {
    x
  } 
## Set Inverse - unused now  
  setInverse<-function(inverse) {
    invMatrix<<-inverse
  }
        
##  getInverse function
  getInverse<-function()  {
    invMatrix
  }
 ## List of all functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)       

}


## This function computes the inverse of the special "matrix"  returned by makeCacheMatrix aboveby using "solve" function . 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the inverse matrix       
  inv<-x$getInverse()
  ##Check and if not NULL return from Cache else get the matrix and create Inverse matrix and set it        
  if(!is.null(inv)){
    message("Matrix is in the Cache")
    return(inv) 
  }
  ## not in Cache, need to create Inverse matrix from the original matrix        
  message("Matrix is not in the cache")
  data<-x$get()
  ## Create inverse matrix         
  inv<-solve(data)
  ## Set Inverse matrix so that this can be used later        
  x$setInverse(inv)
}


