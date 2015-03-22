##makeCacheMatrix is used to set the matrix, get the matrix, set the inverse
##of the matrix and get the inverse of the matrix

##cacheSolve checks if the inverse of the matrix already exists. If it does,
##it returns the inverse or else it creates the inverse and returns it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}