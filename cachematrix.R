## 
## The goal of this programming assignment is to use the 
## <<- operator to cache data for operations which may 
## may be time consuming.  
##
## There are two required functions within this code - 
##
## makeCacheMatrix which will create a special matrix
## object that can cache its inverse.
##
## cacheSolve returns the inverse of a given matrix.
## If the inverse of a matrix has already been cached
## it will return the cached version.
##
## Further details about this assignment are available
## at: https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## 

## makeCacheMatrix creates a special matrix object
## which will cache the inverse of the matrix if
## it has not yet been calculated.  Further 
## requests to invert the matrix will return
## the cached version for performance reasons.
## 
## This function takes one argument:
## 'x' - This is an existing matrix or,
## if this parameter is not specified, a
## default matrix() object will be used.
##
## This function returns a matrix object with 
## the ability to cache its result.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function(y) m
  list (
  	set = set,
	get = get, 
	setsolve = setsolve,
	getsolve = getsolve
	)
}


## cacheSolve works with a special matrix object
## created by makeCacheMatrix to cache the results
## of a matrix inversion operation using solve().
## If the inverse of the matrix has been calculated
## using cacheSolve() previously, the cache result
## will be returned.  If not, the inverse of the
## matrix will be calculated, cached and returned.
##
## This function has one required parameter:
##
## 'x' is a special matrix object created using
## the makeCacheMatrix() function.  
##
## Other parameters will be passed to the solve()
## function.
##
## Note - this function assumes the matrix
## will always be invertible per the 
## assignment.
##
## This function returns a matrix that is 
## the inverse of 'x'.  If the inverse has
## been cached, a message will be printed to
## the console indicating the cached representation
## is being used.

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve( data, ... )
	x$setsolve(m)
	m
}
