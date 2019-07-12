## This functions create a special "matrix" throught we can obtain its inverse.

## Create an object as matrix
## Set the function giving it parameters x and m with and create a matrix that can cache his inverse and store it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set      = set, 
       get      = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculate matrix inverse, if stored skip calculation. The return is the inverse of the matrix.

cacheSolve <- function(x, ...) {

  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

