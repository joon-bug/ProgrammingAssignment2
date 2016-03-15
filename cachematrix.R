##Assignment 3
makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #this function allows you to set the value x (the defined variable in the function) as
  #another value 'y'
  get <- function() x
  setinverse <- function(solve) m <<- solve
  #setinverse sets the value of the inverse of matrix x
  getinverse <- function() m
  #this returns the value of m, which is the inverse of matrix x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  #this is a list of the functions in makeCacheMatrix, which sets up the ability
  #to subset this function and call a specific function in this list.
}

cacheSolve <- function(x, ...) {
  z<-makeCacheMatrix(x)
  m <- z$getinverse()
  #z$getinverse() is subsetting function makeCacheMatrix for the getinverse function.
  #this function returns the value of m, which was been globally defined as the
  #inverse matrix for matrix x.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #provided that the inverse has been properly stored in the first function (therefore
  #not returning an NA value), this function will print "getting cahed data", followed
  #by printing the inverse matrix.
  
  #what follows here is the 'else' portion of an if function. if m is na (indicating
  #that the inverse matrix has NOT been cached, then the remaining code will
  #calculate and then print the inverse matrix)
  data <- z$get()
  m <- solve(data, ...)
  z$setinverse(m)
  m
}