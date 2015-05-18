## These two functions makeCacheMatrix() and cacheSolve() are used to cache and 
## then compute the inverse of an invertible matrix.

## makeCacheMatrix() function creates a special matrix object which can cache its inverse
## which can further be used in cacheSolve() Function.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    	set <- function(y) {
        		x <<- y;
        		inverse <<- NULL;
    	}
    	get <- function() return(x);
   	setinv <- function(inv) inverse <<- inv;
 	getinv <- function() return(inverse);
	return(list(set = set, 
	        get = get, 
	        setinv = setinv, 
	        getinv = getinv))
}

## CacheSolve() function return the inverse of the matrix by computing the special matrix 
## returned by makeCacheMatrix() function above. 
## If the inverse has already been calculated and the matrix is unchanged then
## cacheSolve() functtion will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("Fetching cached data...please wait !!")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        return(inverse)
}
