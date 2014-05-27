## This file defined functions that does cacheing of matraix inverse

## This function creates a cached matrix object that supports cacheing of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
          x <<- y
          m_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m_inverse <<- inverse
  getInverse <- function() m_inverse
  list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


##This function tries to get inverse of a cached Matrix. 
## It tries to get from cache first, if not, creates a 
## new one and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    x$setInverse(solve(data, ...))
    x$getInverse()
}
