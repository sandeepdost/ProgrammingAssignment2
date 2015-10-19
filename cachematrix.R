## Put comments here that give an overall description of what your
## functions do

## Creates a special "vector", which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
        get <- function() x
        setInverseMatrix <- function(iMatrix) m <<- iMatrix
        getInverseMatrix <- function() m
        list(set = set, get = get,
				setInverseMatrix = setInverseMatrix,
				getInverseMatrix = getInverseMatrix)
}


## Following function calculates the Inverse Matrix of the special "vector" 
## created with the above function.

cacheSolve <- function(x, ...) {
		m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
