## Script contains two functions. makeCacheMatrix returns a list object which is intended to
## store a matrix and cache the inverse of that matrix. cacheSolve attempts to retrieve the 
## cached inverse of the matrix and will solve and cache the inverse if it has not been cached already. 

## Creates a list object which contains four functions: set, get, setinverse, getinverse.
## set(y) stores a matrix y. y is expected to be a square, invertible matrix.
## get() returns the matrix y previously stored.
## setinverse(inverse) caches inverse as the variable i (initialized as NULL)
## getinverse() returns previously cached i value (returns NULL if not yet cached)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## expects list output by makeCacheMatrix() as input. calls getinverse function to check
## if inverse value has already been cached. If getinverse returns NULL, uses solve() function
## to calculate inverse, caches the inverse value, and returns that value. Otherwise, returns 
## cached inverse value.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
