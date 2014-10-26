## This function puts the inverse of a Matrix.
## If the evaluation was already made, the function gives this result
## Otherwise, it solves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## This is the builder function. It calls the first function, if necessary
## and gives the resultant inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("calculating data")
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}

## Usage: cacheSolve(makeCacheMatrix(matrix(2*diag(3),3,3)))

