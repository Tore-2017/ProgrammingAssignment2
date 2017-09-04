## Put comments here that give an overall description of what your
## functions do

## The function below caches the inverse value of the function which can be used at a later time with another function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInv(m)
  m
}
