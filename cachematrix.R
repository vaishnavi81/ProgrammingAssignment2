## This program will calculate the inverse of a square matrix and cache the result for future use
## this will save the user a lot of processing speed rather than re calculating the inverse every time

## this function defines the get and set functionality for the inverse matrix, it will either 
## set the inverse if it has not already been calculated or get it if it has already
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will retrieve the inverse of a square matrix if it has already been calculated, 
## and it will calculate it if not
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
