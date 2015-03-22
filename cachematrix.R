## The following functions calculate, and then cache, the inverse of a matrix

## makeCacheMatrix: creates a list object with a function to...
##    1) set the matrix values
##    2) get the matrix values
##    3) set the inverted matrix values
##    4) get the inverted matrix values

makeCacheMatrix <- function(x = matrix()) {
  #create variable for the inverted matrix
  inv <- NULL
  #set the values of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #retrieve the original matrix values
  get <- function() x
  #calculate the inverted matrix
  setinv <- function(solve) inv <<- solve
  #retrieve inverted matrix
  getinv <- function() inv
  #create list object with function to calculate and/or retrieve the inverted matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: calculates and returns the inverse of the matrix created in the
## makeCacheMatrix function, if the inverse hasn't already been
## calculated and cached. if the inverse is cached, the cached inverted
## matrix is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##get the data from the matrix
  data <- x$get()
  ##calculate the inverse
  inv <- solve(data, ...)
  ##cache the inverse
  x$setinv(inv)
  inv
}