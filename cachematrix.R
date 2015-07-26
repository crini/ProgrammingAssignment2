##  The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## 'x' is a numeric matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) inv <<- inverse
  getmatrix <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve checks if matrix 'x' inverse previously calculated
## If yes, obtains the matrix inverse from cache
## If no, returns matrix that is inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
