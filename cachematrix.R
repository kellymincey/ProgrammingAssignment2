## Make a cache of computed inverses of matrixes and use to quickly
## pull the inverse of a given matrix from the cache or compute the inverse
## of a new matrix and store in the created cache

## Create a list of functions that set a matrix, get the matrix, 
## set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv) 
}

## Return a matrix that is the inverse of matrix "x" from above function

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  # if the inverse has already been cached, return it from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # if not, calculate the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  # sets the value of the inverse in the cache
  x$setinv(inv)
  return(inv)
}