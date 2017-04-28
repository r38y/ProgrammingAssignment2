# Set of functions for making a cache-aware matrix and
# calculating the inverse of the matrix in a cache-aware way

# Wraps a plain old matrix with inverse caching capabilities
# You can set + get the uncached matrix and set + get
# the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(ni) i <<- ni
  getinverse <- function() i

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Takes a cacheable matrix (made with makeCacheMatrix) and
# 1) If the inverse is cached, return it, or
# 2) If the inverse is not cached, calculate it, cache it, and return it
# It can be tested with:
# m <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
# cacheable_matrix = makeCacheMatrix(m)
# cacheSolve(cacheable_matrix)
# cacheSolve(cacheable_matrix)
# The first time you call it you'll see the inverse returned
# The second time you call it you'll see the message "Getting cached data"
# and the inverse will be returned.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
