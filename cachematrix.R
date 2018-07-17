## These functions cache the inverse of a matrix.

## This makeCacheMatrix function creates a "matrix" object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## It first checks if the inverse has already been calculated. If so, it gets the result and skips the computation.
## If not, it calculates the inverse and sets the value in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}  
