## Caches special matrix
## makeCacheMatrix fuction - 
##(1) Sets argument as special matrix
##(2) Gets special matrix
##(3) Sets the inverse of matrix 
##(4) Gets the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }  
  get <- function() x
  setinvmatrix <- function(inverse) m <<- inverse
  getinvmatrix <- function() m
  list(set = set, get=get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## Returns cached inversed matrix or computes inverse matrix if not already cached
## Square matrix required
## If inverse matrix is already cached, message and inverse matrix is returned
## If matrix is not cached, inverse matrix is solved and returned

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinvmatrix(m)
    m
}
