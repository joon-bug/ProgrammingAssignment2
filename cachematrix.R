##Assignment 3
makeCacheMatrix <- function(x) {
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

cacheSolve <- function(x, ...) {
  z<-makeCacheMatrix(x)
  m <- z$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- z$get()
  m <- solve(data, ...)
  z$setinverse(m)
  m
}