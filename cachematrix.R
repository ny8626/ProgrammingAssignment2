## Put comments here that give an overall description of what your
## functions do
## Calculate inverse of a given matrix. If the inverse was calculated and stored in cache, print the cached inverse.


## Write a short comment describing this function "makeCacheMatrix"
## Set x as matrix in makeCacheMatrix 
## Note: X is assumed that the matrix supplied is always invertible.

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


## Write a short comment describing this function "cacheSolve"
## Calculate inverse of x in cacheSolve which was assigned in makeCacheMatrix.
## Note: To make cacheSolve function easier to understand, I changed x in cacheSolve(x) to cacheSolve(z)
## If x is not an square matrix, it will return error message as "the given matrix was not a square matrix."


cacheSolve <- function(z, ...) {
#	str(z)
   	m <- z$getinverse()
    	if(!is.null(m)) {
            	message("getting cached data")
            	return(m)
    	}
    	data <- z$get()

	if(dim(data)[1] != dim(data)[2]) {
			message("the given matrix was not a square matrix.")
	}

	else{
    	m <- solve(data, ...)
    	z$setinverse(m)
    	m
	}
## Return a matrix that is the inverse of 'x'

}
