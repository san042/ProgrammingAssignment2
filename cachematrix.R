## These two functions makeCacheMatrix() and cacheSolve() are used to cache and 
## then compute the inverse of an invertible matrix.

## makeCacheMatrix() function creates a special matrix object which can cache its inverse
## which can further be used in cacheSolve() Function.

makeCacheMatrix <- function( m = matrix()) {
  inv <- NULL  # set inverse as NULL
  setMatrix <- function(m1) {
    m <<- m1  # overwrite the existing global variable with the passed matrix
  }
  getMatrix <- function() {
    m  # return the matrix
  }
  setSolve <- function(i) {
    inv <<- i # set the inverse using the passed matrix
  }
  getSolve <- function() {
    inv  # return the invers of a matrix
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setSolve = setSolve, getSolve = getSolve)
  # return the list of functions
}

## CacheSolve() function return the inverse of the matrix by computing the special matrix 
## returned by makeCacheMatrix() function above. 
## If the inverse has already been calculated and the matrix is unchanged then
## cacheSolve() functtion will retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
  
  v <- x$getSolve() # Retrieve any existing inverse
  if(!is.null(v)) {
    print("Retrieving cached data ...")
    return(v)
  }
  else {
    data <- x$getMatrix()
    if( det(data) == 0) {
      stop("Since determinant of matrix is 0, Inverse cannot be found")
    }
    else {
      x$setSolve(solve(data)) # set the actual inverse of the given matrix
    }
  }
  v <- solve(data)
  return(v) 
}