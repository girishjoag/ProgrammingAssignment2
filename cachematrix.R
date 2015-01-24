## makeCacheMatrix function is used to create a special matrix that can be cached.
## cacheSolve function is a wrapper for solve(). This function is to be used with
## 'makeCacheMatrix' to create and retrive cached inverse of a matrix.

## makeCacheMatrix is a function that creates a special matrix to store a matrix and its inverse.
## parameter x (default empty matrix): Passed matrix whose inverse needs to be cached.
##  
## variable mx: Used to store inverse of matrix. 
## get(), set(): Methods to fetch and store the matrix (passed as parameter x)
## getinverse(), setinverse(): Methods to fetch and store cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	#print(environment())
	mx = NULL
	set <- function (px) {
		x <<- px
		mx <<- NULL
	}
	get <- function() {
		x
	}
	
	setinverse <- function(ms) {
		mx <<- ms
		message("Inverse was saved")
		#mx
	}
	
	getinverse <- function() {
		mx
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function is a wrapper on solve() function. It checks if a cached version
## of inverse of matrix is available. Does not calculate the inverse if a cached version 
## is available.  
## Paramter x: Special matrix of type 'makeCacheMatrix' whose inverse needs to be calculated

cacheSolve <- function(x, ...) {
	#print(environment())
        ## Return a matrix that is the inverse of 'x'
	ms = x$getinverse()
	if (is.null(ms) ) {
		message("Cache was empty")
		xdata = x$get()
		ms = solve(xdata)
		#print(ms)
		x$setinverse(ms)
	}
	ms
}
