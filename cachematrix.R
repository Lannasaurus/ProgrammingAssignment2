##This pair of functions saves the inverse of a matrix to cache
## in order to avoid running the same calculation twice.

## This function creates a list of functions 
#to save and retrieve the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  nvrs<-NULL
  set<-function(y){
    x<<-y
    nvrs<<-NULL
  }
  get<-function()x
  setnvrs<- function(solve) nvrs<<-solve
  getnvrs<- function()nvrs
  list(set = set, get = get,
       setnvrs = setnvrs,
       getnvrs = getnvrs)  
  
}


## This function either retrieves the stored inverse of the input matrix
## by calling on the output of the the makeCacheMatrix function 
##or it finds the inverse if none is stored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  nvrs <- x$getnvrs()
  if(!is.null(nvrs)) {
    message("getting cached data")
    return(nvrs)
  }
  matrix <- x$get()
  nvrs <- solve(matrix, ...)
  x$setnvrs(nvrs)
  nvrs
}
