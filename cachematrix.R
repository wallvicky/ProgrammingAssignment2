## Put comments here that give an overall description of what your
## functions do
#These two functions together check if there is an invertible matrix in the external 
#environment and if there is not, compute and commit an inverted matrix to this environment


## Write a short comment describing this function
# This function creates a list of functions that can be called upon to:
# 1. Set the value of a matrix
# 2. get the value of a matrix
# 3. Set the value of an inverted matrix
# 4. Get the value of an inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# This function takes a matrix and inverts it.  However, first it checks if the 
#inverted matrix has already been calculated in the external environment.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
