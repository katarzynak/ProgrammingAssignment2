
# makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	#setting the value of the vector
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	# getting the value of the vector
	get <- function() x
	# setting the value of the inverse
	setinverse <- function(inverse) m <<- inverse
	# getting the value of the inverse
	getinverse <- function() m

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


# CacheSolve computes the inverse of the matrix, if the inverse is calculated before, this function returns the result
cacheSolve <- function(x, ...) {
	m <- x$getinverse()

	# if the inverse is calculated, function returns the inverse from the cache
	if(!is.null(m)) {
		message("getting cached data.")
		return(m)
	}
	# calculate the inverse
	data <- x$get()
	m <- solve(data) 
	# cache the inverse
	x$setinverse(m) 
	# return the inverse
	m 
}
