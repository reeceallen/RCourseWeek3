## this function caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  invers <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(invers) {inv <<- invers}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("return cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}