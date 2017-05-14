## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# defines a list containing a matrix and allows for the introduction of its inverse.
# same structure as the sample, renaming for clarity.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#this function checks if the "composite" matrix contains its inverse 
# matrix in the cache. if so, it returns it. If not, it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
  if(!is.null(inv)) {
    # if the inverse exists, checks if it is the inverse of the matrix.
    # first calculates the dimension of the matrix and if "matrix * inv=identity"
    a<-dim(x$get())[1]
    b<-x$get()%*%inv
    if((b==diag(a))[1,1]){
      # if matrix * inverse=identity, gets the cached data and quits.
      message("getting cached data")
      return(inv) 
}
