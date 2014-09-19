## These functions compute the inverse of (invertible) matrices. To save computing time, they first
## check, whether an inverse has already been computed and cached in the past. If this is the case
## the cached inverse is used.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector, get the value of the vector, set the value of the inverse, get the value of the inverse


makeCacheMatrix <- function(M = matrix()) {
  m <- NULL
  set <- function(N) {
    M <<- N
    m <<- NULL
  }
  get <- function() M
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinv function.

cacheSolve <- function(M, ...) {
  m <- M$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- M$get()
  m <- solve(data, ...)
  M$setinv(m)
  m
}
