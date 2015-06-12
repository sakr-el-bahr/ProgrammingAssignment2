## Since matrix inversion is a costly operation 
## these functions are designed to create and maintain
## an object that is able to store the matrix itself along 
## with its inversion in a way that R have to calculate
## inversion of input matris only once

## Function makeCacheMatrix
## Creates a wrapper object for input matrix x, which stores
## matrix itself and its inversion. Wrapper object has
## following methods:
##  - get() 	gets value of x
##  - set(val) 	sets value of x and unsets value of inversion of x
##  - getInv() 	gets inversion of x
##  - setInv()	sets inversion of x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(data) {
		x <- data
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setInv <- function(val) {
		inv <<- val
	}
	getInv <- function() {
		inv
	}

	list(
		get = get,
		set = set,
		getInv = getInv,
		setInv = setInv
	)
}


## Function cacheSolve
## Calculates inversion of input matrix
## and stores the result, so any consequent calls
## to cacheSolve on the same matrix will simply
## return cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
