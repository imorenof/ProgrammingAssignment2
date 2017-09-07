# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
#The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix this function creates a special "matrix" object that can cache its inver

makeCacheMatrix <- function(x = matrix()) {
  inverso <- NULL
  set <- function(y) {
    x <<- y
    inverso <<- NULL
  }
  get <- function() x
  setInverso <- function(inverso) inverso <<- inverso
  getInverso <- function() inverso
  list(set=set, get=get, setInverso=setInverso, getInverso=getInverso)
}


#cacheSolve this function computes the inverse of the special matrix returned by makecachematrix above, if the inverse
# has already been calculated, then the cachesolve function should retrieve the inverse from the cache
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inverso <- x$getInverso()
  if(!is.null(inverso)) {
    message("collecting data.")
    return(inverso)
  }
  data <- x$get()
  inverso <- solve(data, ...)
  x$setInverso(inverso)
  inverso
}
