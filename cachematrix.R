
## Save processing time by caching the inverse of matrices
## the inverse can be used multiple times in further computations
## after solving just once.

## makeCacheMatrix - create an object containing a matrix
##    and functions to initial and retrieve the matrix
##    and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: Check if the inverse has already been computed
##   if so, return the cached inverse, otherwise, compute,
##   cathe, and return the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

### example calls of the function

#m1 = matrix(c(1,2,5,4),nrow=2)
#m2 = matrix(c(1,2,5,4,3,3,2,2,3),nrow=3)

#mym1 = makeCacheMatrix(m1)
#cacheSolve(mym1)

#mym2 = makeCacheMatrix(m2)
#cacheSolve(mym2)
#cacheSolve(mym1)
