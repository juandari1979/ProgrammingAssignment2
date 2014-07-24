## The function makeCacheMatrix creates the special type
## of matrix that can cache its inverse. When a new matrix
## is set via the set function, the cache value is invalidated (set tu NULL)
## The cacheSolve function first gets the cached inverse from
## the matrix created with makeCacheMatrix and if it is not null
## meaning that it has already been calculated and stored (and that
## the matrix has not been changed) then returns that value.
## Otherwise it gets the actual matrix, calculates the inverse and
## calls the function to store the cached inverse in the special matrix

## Creates the special matrix that can cache its inverse. Creates the
## functions necessary to set a new matrix value, get the non-special matrix,
## get and set the inverse. It is important to notice that when a new matrix
## is set the cache is invalidated.

makeCacheMatrix <- function(x = matrix()) {
	##Initialize the inverse as NULL
	c_inv <- NULL
	##Create the set function that keeps the content of the non-special
	##matrix as x. And initializes the cache as NULL
	set <- function(y) {
			x <<- y
			c_inv <<- NULL
	}
	##Create the get funtion that returns the non-special matrix
	get <- function() x
	##Create the set inverse function that stores the inverse in tha cache
	setinverse <- function(inverse) c_inv <<- inverse
	##Create the get inverse function that returns the currently
	##cached inverse
	getinverse <- function() c_inv
	##The actual special matrix object is a list of the functions that
	##can be called to get the inverse or the matrix.
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Reads the currently cached inverse, if it is not NULL then returns it
## otherwise calculates the inverse from the matrix (gotten using get) and
## sets this new inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		c_inv <- x$getinverse()
		if(!is.null(c_inv)) {  ##The inverse was cached
                message("getting cached data")
                return(c_inv) ##Return the cached value
        }
		##The inverse was not cached so get the non-special matrix
        data <- x$get()
		##Calculate the inverse
        c_inv <- solve(data, ...)
		##Set the new inverse in the cache
        x$setinverse(c_inv)
		##Return the inverse
        c_inv
}
