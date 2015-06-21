makeCacheMatrix <- function(x = matrix()) {
	## returns a list containing functions to
	## 1. set the matrix
	## 2. get the matrix
	## 3. set the inverse
	## 4. get the inverse
	##  	
	inv = NULL
	set = function(y) {
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse 
	getinv = function() inv
	list(set=set, 
	     get=get, 
	     setinv=setinv, 
	     getinv=getinv)
}

cacheSolve <- function(x, ...) {
	## returns inverse of the original matrix input to makeCacheMatrix()
	inv = x$getinv()
	if (!is.null(inv)){ 
		message("getting cached data")
		# get the value from the cache
		return(inv)
	}
	
	matdat = x$get()
	inv = solve(matdat, ...)
	message("calculating data")
	# set the value of the inverse in the cache
	x$setinv(inv)
	inv
}