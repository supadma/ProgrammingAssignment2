## The makeCacheMatrix function creates an matrix object that can cache its inverse
## The cacheSolve function computes the inverse if there is no inverse available in cache. Bringing 
## it from cache saves computational time.

## The function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##  The function computes the inverse of a matrix
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated and is in cache, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
