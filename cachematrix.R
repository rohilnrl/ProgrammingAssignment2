## makeCacheMatrix when called accepts a matrix as argument
## and assigns 4 functions to get and set the matrix and
## it's inverse. cacheSolve solves for the inverse of a matrix.

## Accepts a matrix and creates 4 functions to compute
## and display its inverse.
makeCacheMatrix <- function(x = matrix()) {
  x <- x
  i <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Takes a list of the 4 functions (returned by the above
## function) and calculates the matrix. If already calculated
## it returns it from the cache.
cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if (!is.null(i)) {
    print("Getting cached data...")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
