## create a special matrix that has a list of functions
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the inverse matrix
#4.  get the inverse matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## solve for inverse matrix when it is not found in the cache
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
