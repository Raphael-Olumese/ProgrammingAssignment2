## Defining a makeCacheMatrix function and a cacheSolve function using 

##  `makeCacheMatrix`: This function creates a special matrix object that can cache its inverse. 
##   It sets up a mechanism for storing the matrix data and its inverse, enabling efficient computation and retrieval of the inverse when needed.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special matrix object created by `makeCacheMatrix`.
## It first checks if the inverse has already been calculated and stored. If it has, it retrieves the cached inverse to avoid redundant computations; otherwise, it calculates the inverse and caches it for future use.  



cacheSolve <- function(x, ...) {
         inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message('getting cached data')
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
