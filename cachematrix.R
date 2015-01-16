## Function file for Coursera R Programming Course week 3
## There are two functions: makeCacheMatrix & cacheSolve
## It demonstrates the use of caching to allow reuse of a value
## instead of calculating it everytime, thereby, improving
## peformance in instances where same operation on same data needs to
## be done repeteadly.

## makeCacheMatrix is just a function that returns a list of functions:
## 1. set() - initiates the matrix and its inverse
## 2. get() - returns the matrix
## 3. setinvmatrix() - creates the inverse of the matrix
## 4. getinvmatrix() - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinvmatrix <- function(t1) m <<- t1
	getinvmatrix <- function() m
	list (set = set, get = get, 
		setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## cacheSolve uses the cached inverse matrix, if it exists, else it
## creates the inverse and caches it

cacheSolve <- function(x, ...) {
      ## Check if inverse matrix already exists
	m <- x$getinvmatrix()
	if (!is.null(m)) {
		message("getting cached inverse matrix")
		return(m)
	}
	else {
		message("solving new inverse matrix")
		my_matrix <- x$get()
		m <- solve(my_matrix)
		x$setinvmatrix(m)
		m
	}
}
