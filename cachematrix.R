## The Source File for "R Programming: Assignment 2"
# Matrix inversion is usually a costly computation and there may be some benefit to caching the
# inverse of a matrix rather than compute it repeatedly. Here two functions are defined
# that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# * set the value of the matrix 
# * get the value of the matrix 
# * set the value of the matrix inverse
# * get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    mat    <<- y
    matInv <<- NULL
  }
  get    <- function() x
  setInv <- function(iMat) matInv <<- iMat
  getInv <- function() matInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache.

cacheSolve <- function(x) {
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  data   <- x$get()
  matInv <- solve(data)
  x$setInv(matInv)
  matInv  
}
