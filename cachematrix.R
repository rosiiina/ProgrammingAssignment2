## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL # Initialize inverse as NULL
 # Function to set the matrix
 set<-function(y){
   x<<-y
   inv<<-NULL  # Reset inverse when matrix changes
 }
 # Function to get the matrix
 get<-function()x
  # Function to set the inverse
 setinv<-function(inverse)inv<<-inverse
  # Function to get the inverse
 getinv<-function()inv
 list(set=set,get=get,setinv=setinv,getinv=getinv)
}
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
 # If inverse is already cached, return it
  if(!is.null(inv)){
    message("retrieving inverse matrix")
    return(inv)
  }
  # Compute the inverse if not cached
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv) # Cache the inverse
  return(inv) # Return the computed inverse
}
