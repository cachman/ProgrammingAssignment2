# create the matrix
makeCachematrix <- function(x = matrix()) {
  # m is used to store inverse
  m <- NULL
  #use y to set variable of inverse or not
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the created matrix
  get <- function() x
  #create the cached inverse of the marix
  seti <- function(inverse) m <<- inverse
  #get the cached inversed matrix
  geti <- function() m
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}

cacheSolve <- function(x, ...) {
  #get inverse matrix
  m <- x$geti()
  # if it is already cached, return that matrix and show the message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #otherwise, create and get the inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$seti(m)
  m 
}