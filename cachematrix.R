## Calculating inverse of matrix(particularly big matrix) is a costly operation.The aim of this 
## R script is to calculate the inverse of a matrix and cache it so that it wont be calculated
## everytime.

## This function get a matrix as input and returns a special vector containing function which is used
## to set and get matrix , set and get the inverse of the matrix. inverse of the matrix is cached
## in a different enviroment from the enviroment where the function is defined.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the special vector created by makecacheMatrix function as input,returns the
## inverse of the matrix either by taking it from the special vector if already cached or by 
## calculating it if not already cached.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
