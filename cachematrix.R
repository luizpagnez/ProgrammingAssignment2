## Functions to create a cache matrix / inverse matrix 
## as proposed at ProgrammingAssignment2


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
      }
  get <- function() x
  setInversion <- function(solve) m <<- solve
  getInversion <- function() m
  list(
        set = set, get = get,
        setInversion = setInversion,
        getInversion = getInversion
       )
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInversion()               # Try to retrieve from cache
        if(!is.null(m)) {
            message("getting cached data")  # Signal that hit the cache
            return(m)                       # Return the value on cache
            }
        data <- x$get()
        m <- solve(data, ...)               # Do the inversion or read from cache
        x$setInversion(m)                   # Save the result for next call
        m
}

