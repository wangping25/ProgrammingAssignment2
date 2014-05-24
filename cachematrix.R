## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
