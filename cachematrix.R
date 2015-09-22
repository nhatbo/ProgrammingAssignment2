## We are going to write two functions used to help us to cache the inverse of a matrix
## instead of compute it repeadtedly

## The first function creates a special "matrix", whereas the second function caculates
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		invrs <- NULL
		set <- function(y){
			x <<- y
			invrs <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) invrs <<- inverse
		getinverse <- function() invrs
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()
	if(!is.null(invrs)){
		message("getting cached data")
		return(invrs)
	}
	data <- x$get()
	invrs <- solve(data,...)
	x$setinverse(invrs)
	invrs
}
