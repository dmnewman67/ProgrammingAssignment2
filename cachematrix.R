# makeCacheMatrix creates a function to initialize a matrix
# gets the value and solves for the inverse
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mat <<- inv
  getinv <- function() mat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve returns the inverse of the matrix created in makeCacheMatix

cacheSolve <- function(x, ...) {
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("get data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data)
  x$setinv(mat)
  mat
}
