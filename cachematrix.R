## here x is an inversible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverseof the  "matrix"
## created with the makeCacheMatrix function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the
## computation part (which ultimate aim is to find the inverse of the matrix).
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  z<- solve(data, ...)
  x$setinverse(z)
  z
}
## z-holds the computed inverse matrix
## m- holds the cached matrix from the MakeCacheMatrix function
