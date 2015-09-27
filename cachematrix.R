## makeCacheMatrix is a function that is a contains a sets and gets the value of matrix object that 
## is stored in a cache in a separate environment
##

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse ## This function sets the matrix object in a separate environment.
  getInverse <- function() m ## This funcion gets the matrix object stored in the separate environment.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## CacheSolve is a function that computes the inverse of the matrix originally created by 
## makeCacheMatrix above. The function will determine if the inverse has already been 
## calculated and if this resolves as true, it will then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse() ## This function attempts to get the matrix object
  if (!is.null(m)) { ## This function tests to see if the cached data is null.
    message("getting cached data") ## This prints to the console a message if the cached data was found. If found it will retrieve the data from the cache.
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...) ## This function attempts to solves the equation for containing the matrix
  x$setInverse(m) 
  m
}