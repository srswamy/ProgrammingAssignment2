## Matrix inversion is a costly computation. To improve computation time
## for calculating the inverse of a matrix, we introduce caching via
## the two functions makeCacheMatrix and cacheSolve.

## The function creates a matrix object, where the matrix data and inverse can
## be accessed or set using the get and set function calls
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function uses caching by leveraging the properties of the matrix object.
## It checks whether the inverse of a matrix exists; if the inverse exists then
## it returns the cached inverse of the matrix. Otherwise, it calculates the 
## inverse of the matrix and caches the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if (!is.null(m)) {
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
