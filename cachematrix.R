## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
	in <- NULL
	#setting the value of the vector
	set <- function(y){
		x <<- y
		in <<- NULL
	}
	# getting the value of the vector
	get <- function()
	# setting the value of the inverse
	setinverse<-function(inverse) in <<- inverse
	# getting the value of the inverse
	getinverse<-function()in

	list(set = set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)
}


# CacheSolve computes the inverse of the matrix, if the inverse is calculated before, this function returns the result
cacheSolve <- function(x, ...) {
	in <- x$getinverse()

	# if the inverse is calculated, function returns the inverse from the cache
	if(!is.null(in)) {
		message("getting cached data.")
		return(in)
	}
	# calculate the inverse
	data <- x$get()
	in <- solve(data) 
	# cache the inverse
	x$setinverse(in) 
	# return the inverse
	in 
}
