# This R script contains two functions which will help R programmers to
# solve a matrix and cache the result for later use. Please use the function
# makeCacheMatrix to create a cache matrix, and cacheSolve to solve the matrix.
#
# Example usage to create a new cache matrix:
#   m <- matrix(1:4, ncol = 2, nrow = 2)
#   cacheMatrix <- makeCacheMatrix(m)
#
# Use cacheMatrix.get() to get the original matrix:
#   m <- cacheMatrix.get()
#
# Use cacheMatrix.set(m) to update the underlying matrix of the cache matrix:
#   m <- matrix(4:7, ncol = 2, nrow = 2)
#   cacheMatrix.set(m)
#
# Use cacheSolve(cacheMatrix) to solve the underlying matrix:
#   m <- cacheSolve(cacheMatrix)


# Use this function to make a cache matrix upon an existing matrix.
#
# Arguments:
#   x: The original matrix.
#
# Returns:
#   The cache matrix of the original matrix which you can use to solve and
#   cache the result for further use.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


# Use this function to solve a cache matrix.
#
# Arguments:
#   x: The cache matrix created by function makeCacheMatrix().
#
# Returns:
#   Solve the matrix and return the result. If the underlying matrix has been
#   already solved before, the cached result will be returned.
cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
